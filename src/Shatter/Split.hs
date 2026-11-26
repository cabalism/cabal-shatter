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
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC
  ( GhcPs
  , HsDecl (..)
  , IE (..)
  , IEWildcard (..)
  , ImportDecl (..)
  , ImportDeclQualifiedStyle (..)
  , ImportListInterpretation (..)
  , LHsDecl
  , getLocA
  , moduleNameString
  , unLoc
  )
import Numeric (showHex)
import Shatter.Graph (Group (..), Kind (..), Node (..), Used (..), collect, declGroups, declNodes, usedIn)
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
      headerEnd <- header
      let slices = declSlices headerEnd (parsedDecls parsed)
          fragments = map (fragment slices) groups
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

    -- The pragmas above the module's name. The header comment is left
    -- behind, so that editing it doesn't change every fragment.
    pragmas = pragmaLines . maybe [] (\n -> take (n - 1) src) $ parsedNameLine parsed

    -- The last line of the header and imports.
    header = case (parsedImportLines parsed, parsedNameLine parsed) of
      (Just (_, final), _) -> Right final
      (Nothing, Just n) ->
        case [i | (i, l) <- drop (n - 1) $ zip [1 ..] src, "where" `isSuffixOf` unwords (words l)] of
          i : _ -> Right i
          [] -> refuse "has a header whose end can't be found"
      (Nothing, Nothing) -> Right 0

    imports = [(s, importNeeded $ unLoc i) | i <- parsedImports parsed, Just s <- [lineSpan $ getLocA i]]

    decls = Map.fromList . zip [0 :: Int ..] $ parsedDecls parsed

    names :: Map.Map Int String
    names = fragmentNames modName groups

    nameOf gid = fromMaybe (error "shatter: group without a name") $ Map.lookup gid names

    fragment slices g =
      let own = [(nodeId n, s) | n <- groupNodes g, Just s <- [Map.lookup (nodeId n) slices]]
          used = foldMap usedIn [d | n <- groupNodes g, Just d <- [Map.lookup (nodeId n) decls]]
          importBlock = concat [slice s | (s, needed) <- imports, needed used]
          top =
            pragmas
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
      pragmas
        ++ ["module " ++ modName]
        ++ zipWith (\sep m -> "  " ++ sep ++ " module " ++ m) ("(" : repeat ",") fragmentModules
        ++ ["  ) where", ""]
        ++ maybe [] slice (parsedImportLines parsed)

-- | Whether a fragment that mentions the given names could need an import.
--
-- Only two kinds of import can be ruled out without knowing what a module
-- exports: a qualified import whose qualifier is never written, and an
-- import with a list of names none of which is written. An import that
-- brings in everything, hides names, lists a type with @(..)@, or lists
-- nothing (for its instances) is always kept.
importNeeded :: ImportDecl GhcPs -> Used -> Bool
importNeeded d used
  | qualifier `Set.member` usedQualifiers used = True
  | ideclQualified d /= NotQualified = False
  | otherwise = case ideclImportList d of
      Just (Exactly, items)
        | null (unLoc items) || wildcard items -> True
        | otherwise -> any (`Set.member` usedNames used) . Set.toList . usedNames $ usedIn items
      _ -> True
  where
    qualifier = moduleNameString . unLoc . fromMaybe (ideclName d) $ ideclAs d
    wildcard items =
      any (\case IEThingAll{} -> True; _ -> False) (unLoc <$> unLoc items)
        || any (/= NoIEWildcard) (collect items :: [IEWildcard])

-- | The lines of the @{-\# ... \#-}@ pragmas in a module's preamble.
pragmaLines :: [String] -> [String]
pragmaLines = go False
  where
    go _ [] = []
    go inside (l : ls)
      | inside = l : go (not $ "#-}" `isInfixOf` l) ls
      | "{-#" `isPrefixOf` dropWhile (== ' ') l = l : go (not $ "#-}" `isInfixOf` l) ls
      | otherwise = go False ls

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
