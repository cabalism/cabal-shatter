-- | What a run of @--shadow@ remembers about a library, so that the next
-- run reads only the modules that changed.
module Shatter.Cache
  ( Cache (..)
  , Cached (..)
  , Stamp
  , stampFile
  , stampOf
  , readCache
  , writeCache
  , Manifest (..)
  , ManLib (..)
  , Stat
  , statOf
  , readManifest
  , writeManifest
  ) where

import Control.Exception (IOException, evaluate, try)
import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Word (Word64)
import GHC.Fingerprint (Fingerprint (..), fingerprintString, getFileHash)
import GHC.Generics (Generic)
import Shatter.Index (Entry)
import Data.List (sort)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, listDirectory)
import System.FilePath (takeDirectory)

-- | A hash of a file's content, or of anything else that can be shown.
type Stamp = (Word64, Word64)

stampFile :: FilePath -> IO Stamp
stampFile file = do
  exists <- doesFileExist file
  if exists then (\(Fingerprint a b) -> (a, b)) <$> getFileHash file else pure (0, 0)

stampOf :: Show a => a -> Stamp
stampOf x = let Fingerprint a b = fingerprintString (show x) in (a, b)

data Cache = Cache
  { cacheKey :: Stamp
  -- ^ Everything besides the modules' own text that the split depends on.
  -- A run with a different key starts afresh.
  , cacheModules :: Map FilePath Cached
  -- ^ By source file.
  }
  deriving (Generic)

instance Binary Cache

-- | One module as the last run left it.
data Cached = Cached
  { cachedStamp :: Stamp
  -- ^ Of its source, and whether it had an hs-boot file.
  , cachedName :: String
  , cachedEntry :: Entry
  -- ^ What the rest of the package was told about it.
  , cachedFragments :: [String]
  , cachedRefused :: Maybe String
  -- ^ Why it was left whole, if it was.
  }
  deriving (Generic)

instance Binary Cached

-- | A file's size and time, which are quick to ask for. A directory has a
-- hash of the names in it: its time won't do, because an editor that saves
-- by renaming a new file over the old one changes it on every save.
type Stat = (Integer, String)

statOf :: FilePath -> IO Stat
statOf path = do
  file <- doesFileExist path
  dir <- if file then pure False else doesDirectoryExist path
  if
    | file -> (,) <$> getFileSize path <*> (show <$> getModificationTime path)
    | dir -> (,) (-2) . show . stampOf . sort <$> listDirectory path
    | otherwise -> pure (-1, "")

-- | What the last run looked at, so that the next can tell quickly whether
-- anything has changed.
data Manifest = Manifest
  { manArgs :: Stamp
  -- ^ Of the options given.
  , manWatched :: [(FilePath, Stat)]
  -- ^ What every library depends on: the plan, the .cabal files, the
  -- directories mirrored.
  , manLibs :: [ManLib]
  }
  deriving (Generic)

instance Binary Manifest

data ManLib = ManLib
  { mlDir :: FilePath
  , mlFlags :: [(String, Bool)]
  -- ^ The flag settings it was read with.
  , mlFiles :: [(FilePath, Stat)]
  -- ^ Its sources and its macros header.
  }
  deriving (Generic)

instance Binary ManLib

readManifest :: FilePath -> IO (Maybe Manifest)
readManifest = readBinary

writeManifest :: FilePath -> Manifest -> IO ()
writeManifest = writeBinary

-- | The cache in a file, if there is one that can be read.
readCache :: FilePath -> IO (Maybe Cache)
readCache = readBinary

writeCache :: FilePath -> Cache -> IO ()
writeCache = writeBinary

readBinary :: Binary a => FilePath -> IO (Maybe a)
readBinary file = do
  bytes <- try (BS.readFile file) :: IO (Either IOException BS.ByteString)
  pure $ case bytes of
    Left _ -> Nothing
    Right bs -> case decodeOrFail (LBS.fromStrict bs) of
      Right (_, _, x) -> Just x
      Left _ -> Nothing

writeBinary :: Binary a => FilePath -> a -> IO ()
writeBinary file x = do
  createDirectoryIfMissing True $ takeDirectory file
  let bytes = encode x
  _ <- evaluate $ LBS.length bytes
  LBS.writeFile file bytes
