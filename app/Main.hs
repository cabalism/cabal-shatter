module Main (main) where

import Data.List (isPrefixOf)
import Shatter.Parse (parseFile)
import Shatter.Report (report)
import Shatter.Split (Fragment (..), Split (..), WriteResult (..), split, writeSplit)
import System.Environment (getArgs)
import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

data Mode = Report | SplitTo FilePath | ListModules

data Options = Options
  { optMode :: Mode
  , optTop :: Maybe Int
  , optWithGhc :: Maybe FilePath
  , optGhcFlags :: [String]
  , optFiles :: [FilePath]
  }

main :: IO ()
main = do
  args <- getArgs
  -- When run as @cabal shatter@, cabal passes the command name first.
  case parse (Options Report (Just 10) Nothing [] []) (dropWhile (== "shatter") args) of
    Left err -> die $ err ++ "\n" ++ usage
    Right opts
      | null (optFiles opts) -> die usage
      | otherwise -> do
          oks <- mapM (one opts) . reverse $ optFiles opts
          unless (and oks) exitFailure
  where
    -- A module that can't be handled is reported and the rest carry on.
    refused err = False <$ hPutStrLn stderr err

    one opts file = do
      hasBoot <- doesFileExist $ file ++ "-boot"
      if hasBoot
        then refused $ file ++ ": has an hs-boot file, which the splitter does not handle"
        else
          parseFile (optWithGhc opts) (reverse $ optGhcFlags opts) file >>= \case
            Left err -> refused err
            Right parsed -> case optMode opts of
              Report -> True <$ putStrLn (report (optTop opts) parsed)
              ListModules -> either refused ((True <$) . mapM_ (putStrLn . fragmentModule) . splitFragments) $ split parsed
              SplitTo root -> either refused ((True <$) . write root) $ split parsed

    write root s = do
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

parse :: Options -> [String] -> Either String Options
parse opts = \case
  [] -> Right opts
  "--report" : rest -> parse opts{optMode = Report} rest
  "--split" : rest -> parse opts rest
  "--out" : dir : rest -> parse opts{optMode = SplitTo dir} rest
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
    , "       cabal-shatter --split --out SOURCE-DIR [--with-ghc GHC] [-XEXTENSION ...] FILE ..."
    , "       cabal-shatter --list-modules [--with-ghc GHC] [-XEXTENSION ...] FILE ..."
    , ""
    , "--report        print the groups of mutually dependent declarations in each module"
    , "--split         write each module under SOURCE-DIR as a facade and one fragment per group"
    , "--list-modules  print the names of the fragment modules a split would write"
    ]
