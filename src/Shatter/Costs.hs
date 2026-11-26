-- | Compile times per module, read from GHC's timing dumps or from a file
-- written from them.
module Shatter.Costs
  ( timingsUnder
  , readCosts
  , showCosts
  ) where

import Control.Monad (filterM, forM)
import Data.Char (isUpper)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | The time GHC spent on each module, in milliseconds, summed over the
-- @.dump-timings@ files under a directory. Those files are written by
-- @-ddump-timings -ddump-to-file@.
--
-- The build has to be serial: GHC reports the whole process's CPU time, so
-- under @-j@ every phase is charged for the work of the other threads.
timingsUnder :: FilePath -> IO (Map String Double)
timingsUnder dir = do
  files <- filter (".dump-timings" `isSuffixOf`) <$> walk dir
  Map.fromListWith (+) . concat <$> mapM one files
  where
    walk d = do
      entries <- map (d </>) <$> listDirectory d
      dirs <- filterM doesDirectoryExist entries
      (filter (`notElem` dirs) entries ++) . concat <$> forM dirs walk

    -- A file is about one module: the first one named in it.
    one file = do
      phases <- mapMaybe phase . lines <$> readFile file
      pure [(m, sum $ map snd phases) | m <- take 1 $ mapMaybe fst phases]

    -- A line reads @Phase [Module]: alloc=N time=T@.
    phase l = do
      ms <- readMaybe . drop (length "time=") =<< listToMaybe (filter ("time=" `isPrefixOf`) $ words l)
      pure (moduleIn l, ms)

    moduleIn l = case break (== '[') l of
      (_, _ : rest)
        | (name, ']' : _) <- break (== ']') rest
        , any isUpper (take 1 name)
        , ' ' `notElem` name
        , '/' `notElem` name ->
            Just name
      _ -> Nothing

-- | A module and its time on each line.
showCosts :: Map String Double -> String
showCosts costs = unlines [m ++ " " ++ show ms | (m, ms) <- Map.toList costs]

readCosts :: FilePath -> IO (Map String Double)
readCosts file = Map.fromList . mapMaybe entry . lines <$> readFile file
  where
    entry l = case words l of
      [m, ms] -> (,) m <$> readMaybe ms
      _ -> Nothing
