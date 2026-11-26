-- | Rewrite the locations in GHC's messages from the files of a shadow to
-- the files they were split from.
module Shatter.Locate
  ( Places
  , loadPlaces
  , relocate
  ) where

import Control.Monad (filterM, forM)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (isPrefixOf, isSuffixOf, stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import System.Directory
import System.FilePath
import Text.Read (readMaybe)

-- | For the files of a shadow: where each run of lines came from.
data Places = Places
  { placesFiles :: Map FilePath (FilePath, [(Int, Int, Int)])
  -- ^ By file in the shadow: the original file, and for each run of lines
  -- the shadow's line, the original's line and how many lines.
  , placesModules :: Map String String
  -- ^ The module each fragment was split from.
  , placesDirs :: [FilePath]
  -- ^ The directories of the shadow's packages, which GHC's paths are
  -- relative to.
  }

-- | Read the map files of a shadow. The first argument is the project's
-- directory, the second the shadow's, the third the packages' directories
-- within both.
loadPlaces :: FilePath -> FilePath -> [FilePath] -> IO Places
loadPlaces root out packages = do
  found <- concat <$> mapM (\d -> map ((,) d) <$> maps (out </> d)) packages
  entries <- forM found $ \(_, file) -> do
    ls <- map words . lines <$> readFile file
    pure $ case ls of
      ["module", m] : rest ->
        -- The source directory is what is left of the map's path once the
        -- module's own path is taken off the end.
        let modPath x = map (\c -> if c == '.' then pathSeparator else c) x
            srcDir = fromMaybe (takeDirectory file) . stripSuffix (modPath m) $ dropExtension file
            original = root </> makeRelative out srcDir </> modPath m <.> "hs"
            slices =
              Map.fromListWith
                (flip (++))
                [ (x, [(a, b, n)])
                | [x, a', b', n'] <- rest
                , Just a <- [readMaybe a']
                , Just b <- [readMaybe b']
                , Just n <- [readMaybe n']
                ]
         in ( [(srcDir </> modPath x <.> "hs", (original, ss)) | (x, ss) <- Map.toList slices]
            , [(x, m) | x <- Map.keys slices, x /= m]
            )
      _ -> ([], [])
  pure
    Places
      { placesFiles = Map.fromList $ concatMap fst entries
      , placesModules = Map.fromList $ concatMap snd entries
      , placesDirs = map (out </>) packages
      }
  where
    stripSuffix suffix x = reverse <$> stripPrefix (reverse suffix) (reverse x)

    -- The map files under a directory, not following links or going into
    -- build directories.
    maps dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then pure []
        else do
          entries <- map (dir </>) . filter (not . ("dist-newstyle" `isPrefixOf`)) <$> listDirectory dir
          real <- filterM (fmap not . pathIsSymbolicLink) entries
          dirs <- filterM doesDirectoryExist real
          (filter (".shatter-map" `isSuffixOf`) real ++) . concat <$> mapM maps dirs

-- | Rewrite the lines of GHC's output. A location in a file of the shadow
-- becomes the place in the original that the line was copied from, with
-- the path shown relative to the first argument where it is beneath it. A
-- location on a line the splitter wrote itself, such as a fragment's
-- imports, is left as it is. Fragment module names become the names of the
-- modules they were split from.
relocate :: FilePath -> Places -> [String] -> [String]
relocate cwd places = go Nothing
  where
    -- The slices of the file the last message was about, for the line
    -- numbers in the source excerpt under it.
    go _ [] = []
    go current ls@(l : rest)
      | isGutter l =
          let (excerpt, after) = span isGutter ls
           in gutters current excerpt ++ go current after
      | otherwise =
          let (l', seen) = locations l
              current' = case seen of
                s : _ | not (null l) && not (isSpace $ head' l) -> Just s
                _ -> current
           in modules' (lineHints current' l') : go current' rest

    head' = fromMaybe ' ' . listToMaybe

    -- GHC points at another place in the same file as @(line 12)@.
    lineHints current x = case (current, breakOn "(line " x) of
      (Just slices, Just (before, after))
        | Just (n, ')' : more) <- number after ->
            before ++ "(line " ++ show (fromMaybe n $ lineIn slices n) ++ ")" ++ lineHints current more
        | otherwise -> before ++ "(line " ++ lineHints current after
      _ -> x
    modules' = unwordsBy fragmentName

    -- @123 | code@, or the bare @|@ above and below.
    isGutter l = case span isDigit (dropWhile isSpace l) of
      (_, ' ' : '|' : _) -> True
      ("", '|' : _) -> all isSpace (takeWhile (/= '|') l) && not (null l)
      _ -> False

    gutters current excerpt =
      let parts = [(readMaybe n :: Maybe Int, dropWhile isSpace r) | l <- excerpt, let (n, r) = span isDigit (dropWhile isSpace l)]
          moved = [(fmap (\k -> fromMaybe k $ current >>= (`lineIn` k)) n, r) | (n, r) <- parts]
          width = maximum $ 0 : [length (show n) | (Just n, _) <- moved]
          pad n = replicate (width - length n) ' ' ++ n
       in [pad (maybe "" show n) ++ " " ++ r | (n, r) <- moved]

    lineIn :: [(Int, Int, Int)] -> Int -> Maybe Int
    lineIn slices n = listToMaybe [b + (n - a) | (a, b, k) <- slices, n >= a, n < a + k]

    -- Every @path.hs:line@ on a line, rewritten where the path is a file
    -- of the shadow; and the slices of those files.
    locations l = case breakOn ".hs:" l of
      Nothing -> (l, [])
      Just (before, after) ->
        let (keep, pathStart) = pathBefore before
            path = pathStart ++ ".hs"
            (rest', seenRest) = locations after
         in case lookupFile path of
              Just (original, slices)
                | Just (loc', remainder) <- position slices after ->
                    let (r, s) = locations remainder
                     in (keep ++ shown original ++ ":" ++ loc' ++ r, slices : s)
              _ -> (keep ++ path ++ ":" ++ rest', seenRest)

    -- A path ends at the last character before it that can't be part of one.
    pathBefore before =
      let (p, k) = span (\c -> not (isSpace c) && c `notElem` "‘’'\"()") $ reverse before
       in (reverse k, reverse p)

    lookupFile path =
      listToMaybe . mapMaybe (`Map.lookup` placesFiles places) $
        if isAbsolute path then [normalise path] else [normalise $ d </> path | d <- placesDirs places]

    shown original
      | (cwd ++ [pathSeparator]) `isPrefixOf` original = makeRelative cwd original
      | otherwise = original

    -- @12:3@, @12:3-9@ or @(12,3)-(14,9)@ with its lines moved; nothing if
    -- a line isn't one that was copied.
    position slices after = case after of
      '(' : more -> do
        (l1, ',' : m1) <- number more
        (c1, ')' : '-' : '(' : m2) <- number m1
        (l2, ',' : m3) <- number m2
        (c2, ')' : m4) <- number m3
        a <- lineIn slices l1
        b <- lineIn slices l2
        pure ("(" ++ show a ++ "," ++ show c1 ++ ")-(" ++ show b ++ "," ++ show c2 ++ ")", m4)
      _ -> do
        (l1, more) <- number after
        a <- lineIn slices l1
        pure (show a, more)

    number :: String -> Maybe (Int, String)
    number x = case span isDigit x of
      ("", _) -> Nothing
      (ds, more) -> (,) <$> readMaybe ds <*> pure more

    breakOn needle hay = search "" hay
      where
        search _ [] = Nothing
        search acc x@(c : cs)
          | needle `isPrefixOf` x = Just (reverse acc, drop (length needle) x)
          | otherwise = search (c : acc) cs

    -- A fragment's name, or a name qualified with one.
    fragmentName w = case Map.lookup w (placesModules places) of
      Just m -> m
      Nothing -> case break (== '.') (reverse w) of
        (end, '.' : start)
          | not (null end)
          , let prefix = fragmentName (reverse start)
          , prefix /= reverse start ->
              prefix ++ "." ++ reverse end
        _ -> w

    -- Apply a function to each run of characters that can be a module name.
    unwordsBy f x = case span nameChar x of
      ("", "") -> ""
      ("", c : cs) -> c : unwordsBy f cs
      (w, more) -> f w ++ unwordsBy f more
    nameChar c = isAlphaNum c || c `elem` "._'"
