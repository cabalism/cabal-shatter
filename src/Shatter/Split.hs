-- | Write a module out as one fragment module per declaration group, plus
-- a facade with the original name and exports.
module Shatter.Split
  ( Fragment (..)
  , Split (..)
  , Plan (..)
  , split
  , plan
  , planEntry
  , render
  , writeSplit
  , WriteResult (..)
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_, unless)
import Data.Char (isAlphaNum, ord)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, sort, sortOn)
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
import Shatter.Graph
  ( Group (..)
  , Item (..)
  , Kind (..)
  , Node (..)
  , Used (..)
  , collect
  , declGroups
  , declNodes
  , usedIn
  , usedQualifiers
  , wrap
  )
import Shatter.Index
  ( Bound (..)
  , Entry (..)
  , Found (..)
  , ImportSpec (..)
  , Info (..)
  , Package (..)
  , allows
  , exportsOf
  , find
  , importSpec
  , newtypes
  , orphanModules
  , picks
  )
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
  , splitDirect :: Int
  -- ^ How many imports of other modules in the package were replaced by
  -- imports of their fragments.
  , splitKept :: [String]
  -- ^ The modules whose imports were kept as written because a name
  -- couldn't be traced, once for each fragment that kept one.
  }

-- | How a module will be split, before any text is written.
data Plan = Plan
  { planParsed :: Parsed
  , planNodes :: [Node]
  , planGroups :: [Group]
  , planNames :: Map.Map Int String
  -- ^ The fragment module for each group.
  , planHeaderEnd :: Int
  -- ^ The last line of the header and imports.
  }

-- | Split a parsed module on its own, or say why it can't be split.
split :: Parsed -> Either String Split
split = fmap (render Nothing) . plan

-- | Work out the groups and their names, or say why the module can't be
-- split.
plan :: Parsed -> Either String Plan
plan parsed
  | not (null splices) = refuse $ "has top-level splices at lines " ++ linesOf splices
  | otherwise = do
      headerEnd <- header
      pure
        Plan
          { planParsed = parsed
          , planNodes = nodes
          , planGroups = groups
          , planNames = fragmentNames modName groups
          , planHeaderEnd = headerEnd
          }
  where
    modName = parsedName parsed
    src = parsedSource parsed
    nodes = declNodes modName (parsedDecls parsed)
    groups = sortOn groupId $ declGroups nodes
    splices = filter ((== Splice) . nodeKind) nodes
    linesOf = intercalate ", " . map (show . nodeFirstLine)
    refuse why = Left $ modName ++ ": " ++ why ++ ", which the splitter does not handle"

    header = case (parsedImportLines parsed, parsedNameLine parsed) of
      (Just (_, final), _) -> Right final
      (Nothing, Just n) ->
        case [i | (i, l) <- drop (n - 1) $ zip [1 ..] src, "where" `isSuffixOf` unwords (words l)] of
          i : _ -> Right i
          [] -> refuse "has a header whose end can't be found"
      (Nothing, Nothing) -> Right 0

-- | What the rest of the package needs to know about a planned module.
planEntry :: Plan -> (String, Entry)
planEntry p =
  ( parsedName $ planParsed p
  , Known
      Info
        { infoBinders =
            Map.fromListWith
              (flip (++))
              [ (itemName i, [Bound (name g) (itemHead i) (itemType i) (itemParent i) (itemNewtype i)])
              | g <- planGroups p
              , n <- groupNodes g
              , i <- nodeItems n
              ]
        , infoExports = exportsOf (parsedName $ planParsed p) . parsedExports $ planParsed p
        , infoImports = importSpec . unLoc <$> parsedImports (planParsed p)
        , infoOrphans = [name g | g <- planGroups p, not . null $ groupOrphan g]
        }
  )
  where
    name g = fromMaybe (error "shatter: group without a name") . Map.lookup (groupId g) $ planNames p

-- | Write out a planned module. Given the package's index, a fragment
-- imports the fragments of other modules that bind the names it uses,
-- where it would otherwise import those modules whole.
render :: Maybe Package -> Plan -> Split
render index p =
  Split
    { splitModule = modName
    , splitFacade = facade
    , splitFragments = [f | (f, _, _) <- rendered]
    , splitDirect = sum [n | (_, n, _) <- rendered]
    , splitKept = concat [k | (_, _, k) <- rendered]
    }
  where
    parsed = planParsed p
    modName = parsedName parsed
    src = parsedSource parsed
    groups = planGroups p
    slices = declSlices (planHeaderEnd p) (parsedDecls parsed)
    rendered = map fragment groups

    slice (first, final) = take (final - first + 1) $ drop (first - 1) src

    -- The pragmas above the module's name. The header comment is left
    -- behind, so that editing it doesn't change every fragment.
    pragmas = pragmaLines . maybe [] (\n -> take (n - 1) src) $ parsedNameLine parsed

    imports = [(s, unLoc i) | i <- parsedImports parsed, Just s <- [lineSpan $ getLocA i]]

    decls = Map.fromList . zip [0 :: Int ..] $ parsedDecls parsed

    locals = Set.fromList $ concatMap nodeBinds (planNodes p)

    nameOf gid = fromMaybe (error "shatter: group without a name") . Map.lookup gid $ planNames p

    -- The orphan instances that the module could see through its imports.
    orphans = case index of
      Nothing -> []
      Just pkg ->
        [ "import " ++ o ++ " ()"
        | o <- Set.toList . Set.fromList $ concatMap (orphanModules (packageIndex pkg) . importModule . importSpec . snd) imports
        , o /= modName
        ]

    fragment g =
      let own = [(nodeId n, s) | n <- groupNodes g, Just s <- [Map.lookup (nodeId n) slices]]
          used = foldMap usedIn [d | n <- groupNodes g, Just d <- [Map.lookup (nodeId n) decls]]
          resolved = [importLines used s d | (s, d) <- imports]
          top =
            pragmas
              ++ [ "{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-missing-export-lists -Wno-dodgy-imports -Wno-orphans #-}"
                 , "module " ++ nameOf (groupId g) ++ " where"
                 , ""
                 ]
              ++ concat [ls | (ls, _, _) <- resolved]
              ++ orphans
              ++ defaults
              ++ [sibling $ nameOf d | d <- sort . nub $ groupDeps g]
          place (line, acc, text) (_, s@(first, final)) =
            let body = slice s
             in (line + length body + 1, (line + 1, first, final - first + 1) : acc, text ++ [""] ++ body)
          (_, placed, textLines) = foldl place (length top, [], top) own
       in ( Fragment
              { fragmentModule = nameOf (groupId g)
              , fragmentText = unlines textLines
              , fragmentSlices = reverse placed
              }
          , length [() | (_, True, _) <- resolved]
          , concat [k | (_, _, k) <- resolved]
          )

    -- The lines for one of the module's imports in a fragment, whether it
    -- was replaced by imports of fragments, and the module if it had to be
    -- kept as written.
    importLines used s d = case index of
      Just pkg
        | Just (Known _) <- Map.lookup (importModule spec) (packageIndex pkg)
        , not (importSource spec) ->
            case direct pkg locals used spec of
              Just ls -> (ls, True, [])
              Nothing -> (asWritten, False, [importModule spec | importNeeded d used])
      _ -> (asWritten, False, [])
      where
        spec = importSpec d
        asWritten = if importNeeded d used then slice s else []

    -- @default@ declarations apply to a whole module, so every fragment
    -- gets a copy.
    defaults = concat [slice s | d <- parsedDecls parsed, DefD{} <- [unLoc d], Just s <- [lineSpan $ getLocA d]]

    -- A fragment is imported under the original module's name as well, so
    -- that names qualified with it still resolve, and so that an export of
    -- @module M@ from the facade covers what the fragments define.
    sibling m = "import " ++ m ++ " as " ++ modName

    facade =
      unlines $
        ["{-# OPTIONS_GHC -Wno-unused-imports #-}"]
          ++ (if parsedHasExports parsed then take (planHeaderEnd p) src else generatedHeader)
          ++ map (sibling . nameOf . groupId) groups

    -- With no export list the original exported everything it defined.
    generatedHeader =
      pragmas
        ++ ["module " ++ modName ++ " (module " ++ modName ++ ") where", ""]
        ++ maybe [] slice (parsedImportLines parsed)

-- | Imports of the fragments that bind the names a fragment uses from one
-- import of a split module. 'Nothing' when one of those names might come
-- from outside the package, or needs import syntax this program doesn't
-- write; the import is then kept as it was.
--
-- A name written without a qualifier is only looked for when the module
-- doesn't bind it itself, and only when the import's own list lets it in.
--
-- Every newtype the import brings in is imported too, used or not:
-- coercing through a newtype needs its constructor in scope.
direct :: Package -> Set.Set String -> Used -> ImportSpec -> Maybe [String]
direct pkg locals used spec
  | any (foundUnsure . snd) founds = Nothing
  | otherwise = do
      entries <- traverse entry bounds
      pure
        [ concat
            [ "import "
            , if importQualifiedOnly spec then "qualified " else ""
            , m
            , " as "
            , importQualifier spec
            , " ("
            , intercalate ", " $ Set.toList items
            , ")"
            ]
        | (m, items) <- Map.toList $ Map.fromListWith (<>) [(m, Set.singleton item) | (m, item) <- entries]
        ]
  where
    qualified = [x | (q, x) <- Set.toList $ usedQualified used, q == importQualifier spec]
    unqualified
      | importQualifiedOnly spec = []
      | otherwise = filter (`Set.notMember` locals) . Set.toList $ usedUnqualified used
    candidates = filter (allows $ importList spec) . nub $ qualified ++ unqualified
    founds = [(x, find pkg (importModule spec) x) | x <- candidates]
    bounds =
      nub $
        [b | (x, f) <- founds, b <- foundBounds f, listed x b]
          ++ [b | (x, b) <- newtypes pkg (importModule spec), allows (importList spec) x, listed x b, clear x]
    listed = picks $ importList spec

    -- Whether a name can come into scope without clashing with one the
    -- module binds or one the import leaves out.
    clear c = (importQualifiedOnly spec || c `Set.notMember` locals) && allows (importList spec) c

    -- A type is imported with everything that belongs to it, unless some
    -- of that would clash; then with the rest by name.
    entry b = do
      name <- boundHead b
      pure . (,) (boundModule b) $
        if boundType b
          then
            let children = Map.findWithDefault [] (boundModule b, name) $ packageChildren pkg
             in if all clear children
                  then name ++ "(..)"
                  else name ++ "(" ++ intercalate ", " (map wrap $ filter clear children) ++ ")"
          else wrap name

-- | Whether a fragment that mentions the given names could need an import.
--
-- Only two kinds of import can be ruled out without knowing what a module
-- exports: a qualified import whose qualifier is never written, and an
-- import with a list of names none of which is written. An import that
-- brings in everything, hides names, lists a type with @(..)@, or lists
-- nothing (for its instances) is always kept.
importNeeded :: ImportDecl GhcPs -> Used -> Bool
importNeeded d used
  -- Importing the Prelude in any form turns off the implicit import, so
  -- dropping one would bring every Prelude name back into scope.
  | moduleNameString (unLoc $ ideclName d) == "Prelude" = True
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
      []
        | not (null $ groupOrphan g) -> "orphans"
        | otherwise -> "line" ++ show (nodeFirstLine . minimumOn nodeId $ groupNodes g)
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
