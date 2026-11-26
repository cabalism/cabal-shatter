module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Shatter.Costs (readCosts, showCosts, timingsUnder)
import Shatter.Index (Package, package, textualEntry)
import Shatter.Parse (Cpp (..), parseFile)
import Shatter.Report (report)
import Shatter.Shadow (Library (..), findMacros, ghcVersion, mirror, readLibrary, withFragments)
import Shatter.Split
  ( Fragment (..)
  , Plan
  , Split (..)
  , WriteResult (..)
  , plan
  , planEntry
  , render
  , writeSplit
  )
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist)
import System.FilePath (makeRelative, takeDirectory, (<.>), (</>))
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStr, hPutStrLn, stderr)
import Text.Read (readMaybe)

data Mode = Report | SplitTo FilePath | ListModules | Timings FilePath | Shadow FilePath

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
  , optFiles :: [FilePath]
  }

optCpp :: Options -> Cpp
optCpp opts = Cpp (reverse $ optMacros opts) (reverse $ optCppFlags opts)

main :: IO ()
main = do
  -- When run as @cabal shatter@, cabal passes the command name first.
  args <- dropWhile (== "shatter") <$> getArgs
  case parse (Options Report False Nothing Nothing (Just 10) Nothing [] [] [] "." [] [] []) args of
    Left err -> die $ err ++ "\n" ++ usage
    Right opts
      | Timings dir <- optMode opts -> putStr . showCosts =<< timingsUnder dir
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
  refusals <- forM (zip libs located) $ \(lib, files) -> do
    macros <- findMacros builddirs lib
    let cpp = Cpp (maybe [] pure macros ++ reverse (optMacros opts)) (libCppFlags lib ++ reverse (optCppFlags opts))
        flags = libGhcFlags lib ++ reverse (optGhcFlags opts)
    attempts <- forM files $ \(_, d, f) -> do
      let file = root </> f
      hasBoot <- doesFileExist $ file ++ "-boot"
      result <-
        if hasBoot
          then pure . Left $ file ++ ": has an hs-boot file, which the splitter does not handle"
          else parseFile (optWithGhc opts) cpp flags file
      pure . either (Left . (,,) f file) (Right . (,) d) $ plan pack costs =<< result
    let (whole, plans) = partitionEithers attempts
    index <- packageOf [file | (_, file, _) <- whole] $ map snd plans
    splits <- forM plans $ \(d, p) -> do
      let s = render (Just index) p
      _ <- writeSplit (outAbs </> makeRelative root (libDir lib) </> d) s
      pure s
    -- A module that can't be split is copied, so that the shadow never
    -- holds a link where a later run may write.
    forM_ whole $ \(f, file, _) -> do
      text <- readFile file
      let to = outAbs </> f
      createDirectoryIfMissing True $ takeDirectory to
      same <- do
        exists <- doesFileExist to
        if exists then (== text) <$> (readFile to >>= \t -> length t `seq` pure t) else pure False
      unless same $ writeFile to text
    let fragments = concatMap (map fragmentModule . splitFragments) splits
        cabalFile = libDir lib </> libCabalFile lib
    original <- readFile cabalFile
    case withFragments fragments original of
      Nothing -> die $ cabalFile ++ ": no library section found to add the fragments to"
      Just text -> do
        let to = outAbs </> makeRelative root cabalFile
        same <- do
          exists <- doesFileExist to
          if exists then (== text) <$> (readFile to >>= \t -> length t `seq` pure t) else pure False
        unless same $ writeFile to text
    hPutStrLn stderr . unwords $
      [ libId lib ++ ":"
      , show $ length files
      , "modules,"
      , show $ length fragments
      , "fragments,"
      , show $ length whole
      , "left whole"
      ]
    when (macros == Nothing) . hPutStrLn stderr $
      "  no cabal_macros.h found for it: modules that use CPP may fail to preprocess until it has been built once"
    pure [msg | (_, _, msg) <- whole]
  mapM_ (hPutStrLn stderr . ("  " ++)) $ concat refusals
  hPutStrLn stderr $ "build with: cabal build --project-dir " ++ out

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
    , "--shadow DIR    mirror the project in DIR with links, and split the main library of"
    , "                each PACKAGE-DIR there, with --direct and --pack 150 unless given;"
    , "                the module list, extensions and CPP options come from the package"
    , "--root DIR      the project's directory, when it is not the current one"
    , "--builddir DIR  where to look for the cabal_macros.h of a package that uses CPP;"
    , "                by default dist-newstyle in the shadow, then in the project"
    , "--flag NAME     turn a package flag on, or off with -NAME, when reading the packages"
    , ""
    , "GHC is any of: --with-ghc GHC, -XEXTENSION, --macros HEADER, -DMACRO, -IDIR"
    ]
