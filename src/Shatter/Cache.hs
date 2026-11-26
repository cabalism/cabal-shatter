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
import System.Directory (createDirectoryIfMissing, doesFileExist)
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

-- | The cache in a file, if there is one that can be read.
readCache :: FilePath -> IO (Maybe Cache)
readCache file = do
  bytes <- try (BS.readFile file) :: IO (Either IOException BS.ByteString)
  pure $ case bytes of
    Left _ -> Nothing
    Right bs -> case decodeOrFail (LBS.fromStrict bs) of
      Right (_, _, cache) -> Just cache
      Left _ -> Nothing

writeCache :: FilePath -> Cache -> IO ()
writeCache file cache = do
  createDirectoryIfMissing True $ takeDirectory file
  let bytes = encode cache
  _ <- evaluate $ LBS.length bytes
  LBS.writeFile file bytes
