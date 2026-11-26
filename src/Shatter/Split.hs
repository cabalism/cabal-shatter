-- | Write a module out as one fragment module per declaration group, plus
-- a facade with the original name and exports.
module Shatter.Split
  ( Fragment (..)
  , Split (..)
  , split
  , writeSplit
  , WriteResult (..)
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_, unless)
import Data.Char (isAlphaNum, ord)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import GHC (GhcPs, HsDecl (..), LHsDecl, getLocA, unLoc)
import Numeric (showHex)
import Shatter.Graph (Group (..), Kind (..), Node (..), declGroups, declNodes)
import Shatter.Parse (Parsed (..), lineSpan)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeFile
  )
import System.FilePath (takeDirectory, (<.>), (</>))

-- | One module of the split.
data Fragment = Fragment
  { fragmentModule :: String
  , fragmentText :: String
  , fragmentSlices :: [(Int, Int, Int)]
  -- ^ For each run of lines copied from the original: the fragment's line,
  -- the original's line, and how many lines.
  }

-- | A module split into fragments.
data Split = Split
  { splitModule :: String
  , splitFacade :: String
  , splitFragments :: [Fragment]
  }

-- | Split a parsed module, or say why it can't be split.
split :: Parsed -> Either String Split
split parsed
  | not (null splices) = refuse $ "has top-level splices at lines " ++ linesOf splices
  | not (null orphans) = refuse $ "has orphan instances at lines " ++ linesOf orphans
  | otherwise = do
      (headerEnd, importBlock) <- header
      let slices = declSlices headerEnd (parsedDecls parsed)
          fragments = map (fragment importBlock slices) groups
      pure
        Split
          { splitModule = modName
          , splitFacade = facade headerEnd $ map fragmentModule fragments
          , splitFragments = fragments
          }
  where
    modName = parsedName parsed
    src = parsedSource parsed
    nodes = declNodes modName (parsedDecls parsed)
    groups = sortOn groupId $ declGroups nodes
    splices = filter ((== Splice) . nodeKind) nodes
    orphans = filter nodeOrphan nodes
    linesOf = intercalate ", " . map (show . nodeFirstLine)
    refuse why = Left $ modName ++ ": " ++ why ++ ", which the splitter does not handle"

    slice (first, final) = take (final - first + 1) $ drop (first - 1) src

    -- Lines above the module's name: pragmas and the header comment.
    preamble = maybe [] (\n -> take (n - 1) src) $ parsedNameLine parsed

    -- The last line of the header and imports, and the imports' text.
    header = case (parsedImportLines parsed, parsedNameLine parsed) of
      (Just (first, final), _) -> Right (final, slice (first, final))
      (Nothing, Just n) ->
        case [i | (i, l) <- drop (n - 1) $ zip [1 ..] src, "where" `isSuffixOf` unwords (words l)] of
          i : _ -> Right (i, [])
          [] -> refuse "has a header whose end can't be found"
      (Nothing, Nothing) -> Right (0, [])

    names :: Map.Map Int String
    names = fragmentNames modName groups

    nameOf gid = fromMaybe (error "shatter: group without a name") $ Map.lookup gid names

    fragment importBlock slices g =
      let own = [(nodeId n, s) | n <- groupNodes g, Just s <- [Map.lookup (nodeId n) slices]]
          top =
            preamble
              ++ [ "{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-missing-export-lists #-}"
                 , "module " ++ nameOf (groupId g) ++ " where"
                 , ""
                 ]
              ++ importBlock
              ++ defaults
              ++ ["import " ++ nameOf d | d <- sort $ groupDeps g]
          place (line, acc, text) (_, s@(first, final)) =
            let body = slice s
             in (line + length body + 1, (line + 1, first, final - first + 1) : acc, text ++ [""] ++ body)
          (_, placed, textLines) = foldl place (length top, [], top) own
       in Fragment
            { fragmentModule = nameOf (groupId g)
            , fragmentText = unlines textLines
            , fragmentSlices = reverse placed
            }

    -- @default@ declarations apply to a whole module, so every fragment
    -- gets a copy.
    defaults = concat [slice s | d <- parsedDecls parsed, DefD{} <- [unLoc d], Just s <- [lineSpan $ getLocA d]]

    facade headerEnd fragmentModules =
      unlines $
        ["{-# OPTIONS_GHC -Wno-unused-imports #-}"]
          ++ (if parsedHasExports parsed then take headerEnd src else generatedHeader fragmentModules)
          ++ ["import " ++ m | m <- fragmentModules]

    -- With no export list the original exported everything it defined, so
    -- the facade re-exports every fragment.
    generatedHeader fragmentModules =
      preamble
        ++ ["module " ++ modName]
        ++ zipWith (\sep m -> "  " ++ sep ++ " module " ++ m) ("(" : repeat ",") fragmentModules
        ++ ["  ) where", ""]
        ++ maybe [] slice (parsedImportLines parsed)

-- | The lines each declaration owns: from the line after the previous
-- declaration to its own last line, so that the comments above a
-- declaration travel with it.
declSlices :: Int -> [LHsDecl GhcPs] -> Map.Map Int (Int, Int)
declSlices headerEnd = Map.fromList . go headerEnd . zip [0 ..]
  where
    go _ [] = []
    go prev ((i, d) : rest) = case (unLoc d, lineSpan $ getLocA d) of
      (DocD{}, _) -> go prev rest
      (DefD{}, Just (_, final)) -> go final rest
      (_, Just (first, final)) -> (i, (min first (prev + 1), final)) : go final rest
      (_, Nothing) -> go prev rest

-- | A module name for each group, from the least name it binds. Naming by
-- binder keeps the other fragments' names the same when a declaration is
-- added or moved.
fragmentNames :: String -> [Group] -> Map.Map Int String
fragmentNames modName groups = Map.fromList $ concatMap number byName
  where
    byName = Map.toList $ Map.fromListWith (flip (++)) [(base g, [groupId g]) | g <- groups]
    number (name, [gid]) = [(gid, name)]
    number (name, gids) = [(gid, name ++ "_" ++ show n) | (gid, n) <- zip gids [1 :: Int ..]]
    base g = modName ++ ".S_" ++ case sort (groupNames g) of
      n : _
        | all (\c -> isAlphaNum c || c == '_' || c == '\'') n -> n
        | otherwise -> "op_" ++ concatMap (flip showHex "" . ord) n
      [] -> "line" ++ show (nodeFirstLine . minimumOn nodeId $ groupNodes g)
    minimumOn f = foldr1 (\a b -> if f a <= f b then a else b)

-- | What 'writeSplit' did.
data WriteResult = WriteResult
  { written :: [FilePath]
  , unchanged :: Int
  , removed :: [FilePath]
  }

-- | Write the facade, the fragments and the map under a source directory.
-- A file whose content is the same is left alone, and fragments from an
-- earlier run that no longer exist are removed.
writeSplit :: FilePath -> Split -> IO WriteResult
writeSplit root s = do
  let facadePath = root </> modulePath (splitModule s) <.> "hs"
      fragmentDir = root </> modulePath (splitModule s)
      files =
        (facadePath, splitFacade s)
          : (root </> modulePath (splitModule s) <.> "shatter-map", sliceMap)
          : [(root </> modulePath (fragmentModule f) <.> "hs", fragmentText f) | f <- splitFragments s]
  createDirectoryIfMissing True fragmentDir
  results <- forM files $ \(path, text) -> do
    createDirectoryIfMissing True $ takeDirectory path
    exists <- doesFileExist path
    same <- if exists then (== text) <$> readFileStrict path else pure False
    unless same $ writeFile path text
    pure (path, same)
  dirExists <- doesDirectoryExist fragmentDir
  stale <-
    if dirExists
      then
        filter (`notElem` map fst files)
          . map (fragmentDir </>)
          . filter (\f -> "S_" `isPrefixOf` f && ".hs" `isSuffixOf` f)
          <$> listDirectory fragmentDir
      else pure []
  forM_ stale removeFile
  pure
    WriteResult
      { written = [p | (p, False) <- results]
      , unchanged = length [() | (_, True) <- results]
      , removed = stale
      }
  where
    sliceMap =
      unlines
        [ unwords [fragmentModule f, show line, show orig, show count]
        | f <- splitFragments s
        , (line, orig, count) <- fragmentSlices f
        ]
    readFileStrict path = do
      text <- readFile path
      _ <- evaluate $ length text
      pure text

modulePath :: String -> FilePath
modulePath = map (\c -> if c == '.' then '/' else c)
