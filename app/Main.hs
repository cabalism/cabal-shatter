module Main (main) where

import Data.List (isPrefixOf)
import Shatter.Parse (parseFile)
import Shatter.Report (report)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

data Options = Options
  { optTop :: Maybe Int
  , optWithGhc :: Maybe FilePath
  , optGhcFlags :: [String]
  , optFiles :: [FilePath]
  }

main :: IO ()
main = do
  args <- getArgs
  -- When run as @cabal shatter@, cabal passes the command name first.
  case parse (Options (Just 10) Nothing [] []) (dropWhile (== "shatter") args) of
    Left err -> die $ err ++ "\n" ++ usage
    Right opts
      | null (optFiles opts) -> die usage
      | otherwise -> mapM_ (one opts) . reverse $ optFiles opts
  where
    one opts file =
      parseFile (optWithGhc opts) (reverse $ optGhcFlags opts) file
        >>= either (hPutStrLn stderr) (putStrLn . report (optTop opts))

parse :: Options -> [String] -> Either String Options
parse opts = \case
  [] -> Right opts
  "--report" : rest -> parse opts rest
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
    , ""
    , "Reports the groups of mutually dependent declarations each module would split into."
    ]
