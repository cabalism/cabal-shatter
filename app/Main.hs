module Main (main) where

import Control.Monad (forM, unless, when)
import Data.Either (partitionEithers)
import Data.Set qualified as Set
import Data.List (isPrefixOf, partition, sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Map.Strict qualified as Map
import Data.Map.Lazy qualified as Lazy
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Shatter.Advise (Advice (..), Piece (..), Unit (..), advise, importsOf, showAdvice)
import Shatter.Cache (Cache (..), Cached (..), readCache, stampFile, stampOf, writeCache)
import Shatter.Costs (readCosts, showCosts, timingsUnder)
import Shatter.Index (Entry (..), ImportSpec (..), Info (..), Package, package, textualEntry)
import Shatter.Locate (loadPlaces, relocate)
import Shatter.Parse (Cpp (..), parseFile)
import Shatter.Report (report)
import Shatter.Shadow (Library (..), findMacros, ghcVersion, mirror, readLibrary, withFragments)
import Shatter.Graph (Group (..))
import Shatter.Parse (Parsed (..))
import Shatter.Split
  ( Fragment (..)
  , Plan (..)
  , Split (..)
  , WriteResult (..)
  , plan
  , planEntry
  , render
  , writeSplit
  )
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getFileSize, getModificationTime)
import System.FilePath (makeRelative, takeDirectory, (<.>), (</>))
import System.Environment (getArgs, getExecutablePath)
import System.Exit (die, exitFailure)
import System.IO (BufferMode (..), hPutStr, hPutStrLn, hSetBuffering, stderr, stdout)
import Text.Read (readMaybe)

data Mode = Report | SplitTo FilePath | ListModules | Timings FilePath | Shadow FilePath | Advise | Locate FilePath

data Options = Options
  { optMode :: Mode
  , optDirect :: Bool
  , optPack :: Maybe Double
  , optCosts :: Maybe FilePath
  , optTop :: Maybe Int
  , optWithGhc :: Maybe FilePath
  , optGhcFlags :: [String]
  , optMacros :: [FilePath]
  , optCppFlags :: [String]
  , optRoot :: FilePath
  , optBuildDirs :: [FilePath]
  , optFlags :: [(String, Bool)]
  , optPieces :: Int
  , optSteps :: Int
  , optFiles :: [FilePath]
  }

optCpp :: Options -> Cpp
optCpp opts = Cpp (reverse $ optMacros opts) (reverse $ optCppFlags opts)

main :: IO ()
main = do
  -- When run as @cabal shatter@, cabal passes the command name first.
  args <- dropWhile (== "shatter") <$> getArgs
  case parse (Options Report False Nothing Nothing (Just 10) Nothing [] [] [] "." [] [] 4 0 []) args of
    Left err -> die $ err ++ "\n" ++ usage
    Right opts
      | Timings dir <- optMode opts -> putStr . showCosts =<< timingsUnder dir
      | Locate dir <- optMode opts -> locate dir
      | null (optFiles opts) -> die usage
      | otherwise -> run opts

run :: Options -> IO ()
run opts@Options{optMode = Report} = do
  -- The report needs only a module that parses, not one that can be split.
  costs <- maybe (pure mempty) readCosts $ optCosts opts
  results <- mapM (parseFile (optWithGhc opts) (optCpp opts) (reverse $ optGhcFlags opts)) . reverse $ optFiles opts
  let (failed, parsed) = partitionEithers results
  mapM_ (hPutStrLn stderr) failed
  mapM_ (putStrLn . report (optTop opts) (fromMaybe 0 $ optPack opts) costs) parsed
  unless (null failed) exitFailure
run opts@Options{optMode = Shadow out} = shadow opts out
run opts@Options{optMode = Advise} = advice opts
run opts = do
  costs <- maybe (pure mempty) readCosts $ optCosts opts
  -- A module that can't be handled is reported and the rest carry on.
  attempts <- forM files $ \file -> do
    hasBoot <- doesFileExist $ file ++ "-boot"
    result <-
      if hasBoot
        then pure . Left $ file ++ ": has an hs-boot file, which the splitter does not handle"
        else parseFile (optWithGhc opts) (optCpp opts) (reverse $ optGhcFlags opts) file
    pure . either (Left . (,) file) Right $ plan (fromMaybe 0 $ optPack opts) costs =<< result
  let (whole, plans) = partitionEithers attempts
  mapM_ (hPutStrLn stderr . snd) whole
  case optMode opts of
    Report -> pure ()
    Timings _ -> pure ()
    Shadow _ -> pure ()
    Advise -> pure ()
    Locate _ -> pure ()
    ListModules -> mapM_ (mapM_ (putStrLn . fragmentModule) . splitFragments . render Nothing) plans
    SplitTo root -> do
      index <-
        if optDirect opts
          then Just <$> packageOf (map fst whole) plans
          else pure Nothing
      splits <- forM plans $ \p -> do
        let s = render index p
        result <- writeSplit root s
        hPutStrLn stderr . unwords $
          [ splitModule s ++ ":"
          , show . length $ splitFragments s
          , "fragments,"
          , show . length $ written result
          , "files written,"
          , show $ unchanged result
          , "unchanged,"
          , show . length $ removed result
          , "removed"
          ]
        pure s
      unless (null index) . hPutStr stderr . unlines $
        unwords
          [ "imports within the package:"
          , show . sum $ map splitDirect splits
          , "replaced by imports of fragments,"
          , show . length $ concatMap splitKept splits
          , "kept as written"
          ]
          : [ "  kept " ++ show n ++ " times: " ++ m
            | (m, n) <- take 15 . sortOn (Down . snd) . Map.toList $ Map.fromListWith (+) [(m, 1 :: Int) | m <- concatMap splitKept splits]
            ]
  unless (null whole) exitFailure
  where
    files = reverse $ optFiles opts

-- | Copy standard input to standard output, with locations in the shadow's
-- files rewritten to the files they were split from.
locate :: FilePath -> IO ()
locate out = do
  outAbs <- canonicalizePath out
  let projectFile = outAbs </> ".shatter-cache" </> "project"
  known <- doesFileExist projectFile
  unless known . die $ out ++ ": not a shadow made by --shadow"
  project <- lines <$> readFile projectFile
  case project of
    root : packages -> do
      places <- loadPlaces root outAbs packages
      cwd <- getCurrentDirectory
      hSetBuffering stdout LineBuffering
      mapM_ putStrLn . relocate cwd places . lines =<< getContents
    [] -> die $ projectFile ++ ": empty"

-- | Say which modules of each package to divide for a faster build.
advice :: Options -> IO ()
advice opts = do
  measured <- maybe (pure mempty) readCosts $ optCosts opts
  root <- canonicalizePath $ optRoot opts
  dirs <- mapM canonicalizePath . reverse $ optFiles opts
  let builddirs = case optBuildDirs opts of
        [] -> [root </> "dist-newstyle"]
        ds -> ds
  libs <- forM dirs $ \dir -> either die pure =<< readLibrary ghcVersion (optFlags opts) dir
  results <- forM libs $ \lib -> do
    files <- fmap catMaybes . forM (libModules lib) $ \m -> do
      let path d = libDir lib </> d </> map (\c -> if c == '.' then '/' else c) m <.> "hs"
      found <- mapM (\d -> (,) (path d) <$> doesFileExist (path d)) $ libSourceDirs lib
      pure $ (,) m <$> listToMaybe [f | (f, True) <- found]
    macros <- findMacros builddirs lib
    let cpp = Cpp (maybe [] pure macros ++ reverse (optMacros opts)) (libCppFlags lib ++ reverse (optCppFlags opts))
        flags = libGhcFlags lib ++ reverse (optGhcFlags opts)
    attempts <- forM files $ \(m, file) -> do
      src <- readFile file
      result <- parseFile (optWithGhc opts) cpp flags file
      -- First every group on its own, to size the module; then packed
      -- into about as many pieces as asked for.
      pure $ case plan 0 mempty =<< result of
        Left _ -> Left (m, file, src)
        Right fine ->
          let size = sum . map groupCost $ planGroups fine
           in either (const $ Left (m, file, src)) (\p -> Right (src, fine, size, p)) $
                plan (size / fromIntegral (optPieces opts)) mempty (planParsed fine)
    let (unread, plans) = partitionEithers attempts
    index <- packageOf [file | (_, file, _) <- unread] [p | (_, _, _, p) <- plans]
    let timeOf m estimate = Map.findWithDefault estimate m measured
        units =
          [ Unit m (timeOf m $ 0.4 * fromIntegral (length $ lines src)) (length $ lines src) (importsOf src) [] (importsOf src) 0 []
          | (m, _, src) <- unread
          ]
            ++ [ Unit
                  { unitName = name
                  , unitTime = time
                  , unitLines = parsedLines $ planParsed p
                  , unitImports = importsOf src
                  , unitPieces =
                      [ Piece
                          { pieceName = fragmentModule f
                          , pieceTime = groupCost g * time / max 1 size
                          , pieceLines = groupLines g
                          , pieceBinds = groupNames g
                          , pieceImports = importsOf $ fragmentText f
                          , pieceSingle = groupNames g `elem` map groupNames (planGroups fine)
                          }
                      | length (splitFragments s) > 1
                      , f <- splitFragments s
                      , g <- planGroups p
                      , Map.lookup (groupId g) (planNames p) == Just (fragmentModule f)
                      ]
                  , unitRest = importsOf $ splitFacade s
                  , unitGroups = length $ planGroups fine
                  , unitBinds = concatMap groupNames $ planGroups fine
                  }
               | (src, fine, size, p) <- plans
               , let s = render (Just index) p
                     name = splitModule s
                     time = timeOf name size
               ]
        result = advise (optSteps opts) units
    putStrLn $ showAdvice (libId lib) (any ((`Map.member` measured) . unitName) units) units result
    pure (lib, result)
  -- A package waits for the packages it depends on.
  let finish chain = Lazy.fromList [(libName lib, one lib a) | (lib, a) <- results]
        where
          table = finish chain
          one lib a = fst (chain a) + maximum (0 : mapMaybe (`Lazy.lookup` table) (filter (/= libName lib) $ libDepends lib))
      total chain = maximum . (0 :) . Lazy.elems $ finish chain
      secs :: Double -> String
      secs ms = show (fromIntegral (round (ms / 10) :: Int) / 100 :: Double) ++ " s"
  when (length results > 1) . putStrLn $
    "all packages, each waiting for those it depends on: "
      ++ secs (total adviceBefore)
      ++ " as they stand, "
      ++ secs (total adviceAfter)
      ++ " with the modules divided"

-- | Mirror the project and split the main library of each package given.
shadow :: Options -> FilePath -> IO ()
shadow opts out = do
  costs <- maybe (pure mempty) readCosts $ optCosts opts
  root <- canonicalizePath $ optRoot opts
  createDirectoryIfMissing True out
  outAbs <- canonicalizePath out
  dirs <- mapM canonicalizePath . reverse $ optFiles opts
  let builddirs = case optBuildDirs opts of
        [] -> [outAbs </> "dist-newstyle", root </> "dist-newstyle"]
        ds -> ds
      pack = fromMaybe 150 $ optPack opts
  libs <- forM dirs $ \dir ->
    either die pure =<< readLibrary ghcVersion (optFlags opts) dir
  -- Where each module's source is, for the modules written in plain Haskell.
  located <- forM libs $ \lib ->
    fmap catMaybes . forM (libModules lib) $ \m -> do
      let rel d = makeRelative root (libDir lib) </> d </> map (\c -> if c == '.' then '/' else c) m <.> "hs"
      found <- mapM (\d -> (,) d <$> doesFileExist (root </> rel d)) $ libSourceDirs lib
      pure $ case [d | (d, True) <- found] of
        d : _ -> Just (m, d, rel d)
        [] -> Nothing
  mirror
    root
    outAbs
    [makeRelative root $ libDir lib | lib <- libs]
    (concat [map (makeRelative root (libDir lib) </>) (libSourceDirs lib) | lib <- libs])
    (concat [(makeRelative root (libDir lib) </> libCabalFile lib) : [f | (_, _, f) <- files] | (lib, files) <- zip libs located])
  -- This program by its size and time, which is quicker than hashing it.
  -- For --locate: the project's directory and the packages split.
  createDirectoryIfMissing True $ outAbs </> ".shatter-cache"
  do
    let text = unlines $ root : [makeRelative root $ libDir lib | lib <- libs]
        to = outAbs </> ".shatter-cache" </> "project"
    same <- sameText to text
    unless same $ writeFile to text
  exe <- do
    path <- getExecutablePath
    (,) <$> getFileSize path <*> (show <$> getModificationTime path)
  costsStamp <- maybe (pure (0, 0)) stampFile $ optCosts opts
  refusals <- forM (zip libs located) $ \(lib, files) -> do
    macros <- findMacros builddirs lib
    let cpp = Cpp (maybe [] pure macros ++ reverse (optMacros opts)) (libCppFlags lib ++ reverse (optCppFlags opts))
        flags = libGhcFlags lib ++ reverse (optGhcFlags opts)
        cabalFile = libDir lib </> libCabalFile lib
        cacheFile = outAbs </> ".shatter-cache" </> libId lib
    -- Everything the split depends on besides the text of each module. If
    -- any of it differs from the last run, every module is read again.
    headers <- mapM stampFile $ cppMacros cpp
    cabalStamp <- stampFile cabalFile
    let key = stampOf (exe, costsStamp, headers, cabalStamp, pack, flags, cpp, optWithGhc opts, [f | (_, _, f) <- files])
    old <- readCache cacheFile
    let before = case old of
          Just c | cacheKey c == key -> cacheModules c
          _ -> mempty
    current <- forM files $ \(m, d, f) -> do
      let file = root </> f
      hasBoot <- doesFileExist $ file ++ "-boot"
      (a, b) <- stampFile file
      pure (m, d, f, hasBoot, (a, if hasBoot then b + 1 else b))
    let source (_, _, f, _, _) = f
        readOne x@(m, _, f, hasBoot, _) = do
          let file = root </> f
          result <-
            if hasBoot
              then pure . Left $ file ++ ": has an hs-boot file, which the splitter does not handle"
              else parseFile (optWithGhc opts) cpp flags file
          case plan pack costs =<< result of
            Right p -> pure (x, Right p, planEntry p)
            Left msg -> do
              src <- readFile file
              pure (x, Left msg, fromMaybe (m, Unknown [] False) $ textualEntry src)
        (stale, fresh) = partition (\x@(_, _, _, _, stamp) -> fmap cachedStamp (Map.lookup (source x) before) /= Just stamp) current
    first <- mapM readOne stale
    -- A module whose text changed may now bind or export different names,
    -- or keep them in different fragments. If what the package is told
    -- about it changed, the modules that import it, and those that import
    -- them, are read and written again as well; if not, nothing outside
    -- it can change.
    let told x fragments (name, entry) = case Map.lookup (source x) before of
          Just c -> cachedName c == name && cachedEntry c == entry && cachedFragments c == fragments
          Nothing -> False
        fragmentsOf = either (const []) (map fragmentModule . splitFragments . render Nothing)
        retold = Set.fromList [fst e | (x, r, e) <- first, not $ told x (fragmentsOf r) e]
        importsOf' = \case
          Known info -> map importModule $ infoImports info
          Unknown is _ -> is
        spread seen =
          let more =
                Set.fromList
                  [ cachedName c
                  | x <- fresh
                  , Just c <- [Map.lookup (source x) before]
                  , cachedName c `Set.notMember` seen
                  , any (`Set.member` seen) . importsOf' $ cachedEntry c
                  ]
           in if Set.null more then seen else spread (seen <> more)
        reached = spread retold
        (again, untouched) = partition (\x -> maybe False ((`Set.member` reached) . cachedName) $ Map.lookup (source x) before) fresh
    second <- mapM readOne again
    let readNow = first ++ second
        kept = [(x, c) | x <- untouched, Just c <- [Map.lookup (source x) before]]
        index =
          package . Map.fromList $
            [(cachedName c, cachedEntry c) | (_, c) <- kept] ++ [e | (_, _, e) <- readNow]
    written <- forM readNow $ \(x@(_, d, f, _, stamp), r, (name, entry)) -> case r of
      Right p -> do
        let s = render (Just index) p
        _ <- writeSplit (outAbs </> makeRelative root (libDir lib) </> d) s
        pure (source x, Cached stamp name entry (map fragmentModule $ splitFragments s) Nothing)
      -- A module that can't be split is copied, so that the shadow never
      -- holds a link where a later run may write.
      Left msg -> do
        text <- readFile $ root </> f
        let to = outAbs </> f
        createDirectoryIfMissing True $ takeDirectory to
        same <- sameText to text
        unless same $ writeFile to text
        pure (source x, Cached stamp name entry [] (Just msg))
    let after = Map.fromList $ written ++ [(source x, c) | (x, c) <- kept]
        inOrder = mapMaybe ((`Map.lookup` after) . source) current
        fragments = concatMap cachedFragments inOrder
    original <- readFile cabalFile
    case withFragments fragments original of
      Nothing -> die $ cabalFile ++ ": no library section found to add the fragments to"
      Just text -> do
        let to = outAbs </> makeRelative root cabalFile
        same <- sameText to text
        unless same $ writeFile to text
    writeCache cacheFile $ Cache key after
    hPutStrLn stderr . unwords $
      [ libId lib ++ ":"
      , show $ length files
      , "modules,"
      , show $ length readNow
      , "read,"
      , show $ length fragments
      , "fragments,"
      , show $ length [() | c <- inOrder, Just _ <- [cachedRefused c]]
      , "left whole"
      ]
    when (macros == Nothing) . hPutStrLn stderr $
      "  no cabal_macros.h found for it: modules that use CPP may fail to preprocess until it has been built once"
    pure [msg | (_, Left msg, _) <- readNow]
  mapM_ (hPutStrLn stderr . ("  " ++)) $ concat refusals
  hPutStrLn stderr $ "build with: cabal build --project-dir " ++ out

-- | Whether a file holds this text already.
sameText :: FilePath -> String -> IO Bool
sameText file text = do
  exists <- doesFileExist file
  if exists then (== text) <$> (readFile file >>= \t -> length t `seq` pure t) else pure False

-- | What every module of the package binds, exports and imports. The files
-- that couldn't be split are scanned as text.
packageOf :: [FilePath] -> [Plan] -> IO Package
packageOf whole plans = do
  scanned <- mapMaybe textualEntry <$> mapM readFile whole
  pure . package . Map.fromList $ scanned ++ map planEntry plans

parse :: Options -> [String] -> Either String Options
parse opts = \case
  [] -> Right opts
  "--report" : rest -> parse opts{optMode = Report} rest
  "--split" : rest -> parse opts rest
  "--out" : dir : rest -> parse opts{optMode = SplitTo dir} rest
  "--direct" : rest -> parse opts{optDirect = True} rest
  "--list-modules" : rest -> parse opts{optMode = ListModules} rest
  "--pack" : n : rest
    | Just k <- readMaybe n -> parse opts{optPack = Just k} rest
    | otherwise -> Left $ "--pack needs a number of milliseconds, not " ++ n
  "--costs" : file : rest -> parse opts{optCosts = Just file} rest
  "--timings" : dir : rest -> parse opts{optMode = Timings dir} rest
  "--all" : rest -> parse opts{optTop = Nothing} rest
  "--top" : n : rest
    | Just k <- readMaybe n -> parse opts{optTop = Just k} rest
    | otherwise -> Left $ "--top needs a number, not " ++ n
  "--with-ghc" : ghc : rest -> parse opts{optWithGhc = Just ghc} rest
  "--locate" : dir : rest -> parse opts{optMode = Locate dir} rest
  "--advise" : rest -> parse opts{optMode = Advise} rest
  "--pieces" : n : rest
    | Just k <- readMaybe n, k > 1 -> parse opts{optPieces = k} rest
    | otherwise -> Left $ "--pieces needs a number above one, not " ++ n
  "--steps" : n : rest
    | Just k <- readMaybe n -> parse opts{optSteps = k} rest
    | otherwise -> Left $ "--steps needs a number, not " ++ n
  "--shadow" : dir : rest -> parse opts{optMode = Shadow dir} rest
  "--root" : dir : rest -> parse opts{optRoot = dir} rest
  "--builddir" : dir : rest -> parse opts{optBuildDirs = optBuildDirs opts ++ [dir]} rest
  "--flag" : ('-' : name) : rest -> parse opts{optFlags = (name, False) : optFlags opts} rest
  "--flag" : name : rest -> parse opts{optFlags = (dropWhile (== '+') name, True) : optFlags opts} rest
  "--macros" : file : rest -> parse opts{optMacros = file : optMacros opts} rest
  flag : rest
    | "-D" `isPrefixOf` flag || "-I" `isPrefixOf` flag -> parse opts{optCppFlags = flag : optCppFlags opts} rest
    | "-X" `isPrefixOf` flag -> parse opts{optGhcFlags = flag : optGhcFlags opts} rest
    | "-" `isPrefixOf` flag -> Left $ "unknown option " ++ flag
  file : rest -> parse opts{optFiles = file : optFiles opts} rest

usage :: String
usage =
  unlines
    [ "usage: cabal-shatter --report [--pack MS] [--costs FILE] [--top N | --all] [GHC] FILE ..."
    , "       cabal-shatter --split --out SOURCE-DIR [--pack MS] [--costs FILE] [--direct] [GHC] FILE ..."
    , "       cabal-shatter --list-modules [--pack MS] [--costs FILE] [GHC] FILE ..."
    , "       cabal-shatter --timings DUMP-DIR"
    , "       cabal build --project-dir OUT-DIR 2>&1 | cabal-shatter --locate OUT-DIR"
    , "       cabal-shatter --advise [--costs FILE] [--pieces N] [--steps N] [--root PROJECT-DIR]"
    , "                     [--builddir DIR] [--flag [-]NAME] [GHC] PACKAGE-DIR ..."
    , "       cabal-shatter --shadow OUT-DIR [--root PROJECT-DIR] [--builddir DIR] [--flag [-]NAME]"
    , "                     [--pack MS] [--costs FILE] [GHC] PACKAGE-DIR ..."
    , ""
    , "--report        print the groups of mutually dependent declarations in each module"
    , "--split         write each module under SOURCE-DIR as a facade and one fragment per group"
    , "--direct        with --split, given every module of a package: have a fragment import"
    , "                the fragments of other modules, not those modules whole"
    , "--list-modules  print the names of the fragment modules a split would write"
    , "--pack MS       pack groups into fragments that take at least MS milliseconds to"
    , "                compile; a module that packs into one fragment is left as one module"
    , "--costs FILE    measured compile times to pack by, as written by --timings; groups"
    , "                without one are estimated from their size"
    , "--timings DIR   print the compile time of each module from the .dump-timings files"
    , "                under DIR, for --costs; the build has to be serial and unpacked"
    , "--advise        say which modules of each package's main library to divide, and into"
    , "                what, to shorten the longest chain of modules that wait for each"
    , "                other; --costs gives measured times per module, as --timings writes"
    , "                them from a serial build of the package as it stands"
    , "--pieces N      with --advise, divide a module into about N pieces (default 4)"
    , "--steps N       with --advise, name at most N modules to divide (default: all that help)"
    , "--shadow DIR    mirror the project in DIR with links, and split the main library of"
    , "                each PACKAGE-DIR there, with --direct and --pack 150 unless given;"
    , "                the module list, extensions and CPP options come from the package"
    , "--locate DIR    copy standard input to standard output, rewriting locations in the"
    , "                files of the shadow DIR to the files they were split from"
    , "--root DIR      the project's directory, when it is not the current one"
    , "--builddir DIR  where to look for the cabal_macros.h of a package that uses CPP;"
    , "                by default dist-newstyle in the shadow, then in the project"
    , "--flag NAME     turn a package flag on, or off with -NAME, when reading the packages"
    , ""
    , "GHC is any of: --with-ghc GHC, -XEXTENSION, --macros HEADER, -DMACRO, -IDIR"
    ]
