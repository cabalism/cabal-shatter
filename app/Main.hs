module Main (main) where

import Control.Monad (forM, unless)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Shatter.Index (Package, package, textualEntry)
import Shatter.Parse (parseFile)
import Shatter.Report (report)
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
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStr, hPutStrLn, stderr)
import Text.Read (readMaybe)

data Mode = Report | SplitTo FilePath | ListModules

data Options = Options
  { optMode :: Mode
  , optDirect :: Bool
  , optTop :: Maybe Int
  , optWithGhc :: Maybe FilePath
  , optGhcFlags :: [String]
  , optFiles :: [FilePath]
  }

main :: IO ()
main = do
  -- When run as @cabal shatter@, cabal passes the command name first.
  args <- dropWhile (== "shatter") <$> getArgs
  case parse (Options Report False (Just 10) Nothing [] []) args of
    Left err -> die $ err ++ "\n" ++ usage
    Right opts
      | null (optFiles opts) -> die usage
      | otherwise -> run opts

run :: Options -> IO ()
run opts@Options{optMode = Report} = do
  -- The report needs only a module that parses, not one that can be split.
  results <- mapM (parseFile (optWithGhc opts) (reverse $ optGhcFlags opts)) . reverse $ optFiles opts
  let (failed, parsed) = partitionEithers results
  mapM_ (hPutStrLn stderr) failed
  mapM_ (putStrLn . report (optTop opts)) parsed
  unless (null failed) exitFailure
run opts = do
  -- A module that can't be handled is reported and the rest carry on.
  attempts <- forM files $ \file -> do
    hasBoot <- doesFileExist $ file ++ "-boot"
    result <-
      if hasBoot
        then pure . Left $ file ++ ": has an hs-boot file, which the splitter does not handle"
        else parseFile (optWithGhc opts) (reverse $ optGhcFlags opts) file
    pure . either (Left . (,) file) Right $ plan =<< result
  let (whole, plans) = partitionEithers attempts
  mapM_ (hPutStrLn stderr . snd) whole
  case optMode opts of
    Report -> pure ()
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
  "--all" : rest -> parse opts{optTop = Nothing} rest
  "--top" : n : rest
    | Just k <- readMaybe n -> parse opts{optTop = Just k} rest
    | otherwise -> Left $ "--top needs a number, not " ++ n
  "--with-ghc" : ghc : rest -> parse opts{optWithGhc = Just ghc} rest
  flag : rest
    | "-X" `isPrefixOf` flag -> parse opts{optGhcFlags = flag : optGhcFlags opts} rest
    | "-" `isPrefixOf` flag -> Left $ "unknown option " ++ flag
  file : rest -> parse opts{optFiles = file : optFiles opts} rest

usage :: String
usage =
  unlines
    [ "usage: cabal-shatter --report [--top N | --all] [--with-ghc GHC] [-XEXTENSION ...] FILE ..."
    , "       cabal-shatter --split --out SOURCE-DIR [--direct] [--with-ghc GHC] [-XEXTENSION ...] FILE ..."
    , "       cabal-shatter --list-modules [--with-ghc GHC] [-XEXTENSION ...] FILE ..."
    , ""
    , "--report        print the groups of mutually dependent declarations in each module"
    , "--split         write each module under SOURCE-DIR as a facade and one fragment per group"
    , "--direct        with --split, given every module of a package: have a fragment import"
    , "                the fragments of other modules, not those modules whole"
    , "--list-modules  print the names of the fragment modules a split would write"
    ]
