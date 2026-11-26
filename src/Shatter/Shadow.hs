-- | A shadow of a project: a directory that mirrors it with links, apart
-- from the libraries to be split, whose sources and @.cabal@ files are
-- written afresh. Building in the shadow leaves the project untouched.
module Shatter.Shadow
  ( Library (..)
  , readLibrary
  , ghcVersion
  , withFragments
  , findMacros
  , mirror
  ) where

import Control.Monad (filterM, forM, when)
import Data.ByteString qualified as BS
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf, nub)
import Data.Maybe (fromMaybe, listToMaybe)
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.PackageDescription
  ( BuildInfo (..)
  , ConfVar (..)
  , GenericPackageDescription (..)
  , PackageFlag (..)
  , mkFlagName
  , package
  , pkgName
  , pkgVersion
  )
import Distribution.PackageDescription qualified as PD
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty (prettyShow)
import Distribution.Types.Dependency (depPkgName)
import Distribution.System (buildArch, buildOS)
import Distribution.Types.CondTree (simplifyCondTree)
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Version (Version, mkVersion, withinRange)
import GHC.Settings.Config (cProjectVersion)
import System.Directory
import System.FilePath

-- | What the splitter needs to know about a package's main library.
data Library = Library
  { libDir :: FilePath
  -- ^ The package's directory.
  , libCabalFile :: FilePath
  -- ^ The name of its @.cabal@ file, within that directory.
  , libId :: String
  -- ^ Name and version, as cabal names its build directory.
  , libSourceDirs :: [FilePath]
  -- ^ Relative to the package's directory.
  , libModules :: [String]
  -- ^ The modules it lists, without the generated ones.
  , libGhcFlags :: [String]
  -- ^ The language and extensions it turns on for every module.
  , libCppFlags :: [String]
  -- ^ Its @cpp-options@ and @include-dirs@.
  , libName :: String
  , libDepends :: [String]
  -- ^ The packages it depends on, by name.
  }

-- | The version of GHC this program was built with, which is the one whose
-- parser it uses.
ghcVersion :: Version
ghcVersion = mkVersion . map read . words $ map (\c -> if c == '.' then ' ' else c) cProjectVersion

-- | Read the main library of the package in a directory. Conditionals are
-- decided for this machine, the given compiler version and the given flag
-- settings; a flag that isn't given has its default.
readLibrary :: Version -> [(String, Bool)] -> FilePath -> IO (Either String Library)
readLibrary ghc flags dir = do
  cabals <- filter ((== ".cabal") . takeExtension) <$> listDirectory dir
  case cabals of
    [file] -> do
      parsed <- parseGenericPackageDescriptionMaybe <$> BS.readFile (dir </> file)
      pure $ case parsed of
        Nothing -> Left $ dir </> file ++ ": does not parse"
        Just gpd -> case condLibrary gpd of
          Nothing -> Left $ dir </> file ++ ": has no library"
          Just tree ->
            let lib = simplifyCondTree (decide gpd) tree
                bi = PD.libBuildInfo lib
                generated = autogenModules bi
                pid = package $ PD.packageDescription gpd
             in Right
                  Library
                    { libDir = dir
                    , libCabalFile = file
                    , libId = prettyShow (pkgName pid) ++ "-" ++ prettyShow (pkgVersion pid)
                    , libName = prettyShow $ pkgName pid
                    , libDepends = map (prettyShow . depPkgName) $ targetBuildDepends bi
                    , libSourceDirs = case map getSymbolicPath $ hsSourceDirs bi of
                        [] -> ["."]
                        ds -> ds
                    , libModules = map prettyShow . filter (`notElem` generated) $ PD.exposedModules lib ++ otherModules bi
                    , libGhcFlags =
                        map (("-X" ++) . prettyShow) (maybe [] pure $ defaultLanguage bi)
                          ++ map (("-X" ++) . prettyShow) (defaultExtensions bi)
                    , libCppFlags =
                        filter (\o -> "-D" `isPrefixOf` o || "-U" `isPrefixOf` o) (cppOptions bi)
                          ++ map (("-I" ++) . (dir </>) . getSymbolicPath) (includeDirs bi)
                    }
    [] -> pure . Left $ dir ++ ": has no .cabal file"
    _ -> pure . Left $ dir ++ ": has more than one .cabal file"
  where
    decide gpd = \case
      OS os -> Right $ os == buildOS
      Arch arch -> Right $ arch == buildArch
      Impl flavor range -> Right $ flavor == GHC && withinRange ghc range
      PackageFlag f ->
        Right . fromMaybe False . listToMaybe $
          [on | (name, on) <- flags, mkFlagName name == f]
            ++ [flagDefault pf | pf <- genPackageFlags gpd, flagName pf == f]

-- | The text of a @.cabal@ file with modules added to its main library, as
-- a second @other-modules@ field at the top of the @library@ section,
-- after any @import@ of common stanzas, which has to come first.
-- 'Nothing' when no such section is found.
withFragments :: [String] -> String -> Maybe String
withFragments [] text = Just text
withFragments mods text = case break isLibrary ls of
  (before, lib : after) ->
    let indent = case filter (not . blank) after of
          l : _ | (i@(_ : _), _) <- span (== ' ') l -> i
          _ -> "  "
        (imports, rest) = leading False after
     in Just . unlines $
          before
            ++ [lib]
            ++ imports
            ++ [indent ++ "other-modules:"]
            ++ [indent ++ indent ++ m | m <- mods]
            ++ rest
  _ -> Nothing
  where
    ls = lines text
    isLibrary l = map toLower (trimEnd l) == "library"
    trimEnd = reverse . dropWhile isSpace . reverse
    blank l = all isSpace l || "--" `isPrefixOf` dropWhile isSpace l

    -- The imports that open the section, with the lines that continue
    -- them and the blank lines and comments among them.
    leading continuing = \case
      l : more
        | isImport l || blank l || (continuing && deeper l) ->
            let (xs, rest) = leading (isImport l || (continuing && not (blank l))) more
             in if any isImport (l : xs) || continuing then (l : xs, rest) else ([], l : more)
      rest -> ([], rest)
    isImport l = "import:" `isPrefixOf` map toLower (dropWhile isSpace l)
    deeper l = length (takeWhile (== ' ') l) > baseIndent
    baseIndent = case [length (takeWhile (== ' ') l) | l <- ls, isImport l] of
      n : _ -> n
      [] -> 0

-- | The macros header cabal wrote for a library the last time it was
-- configured, looked for in each build directory in turn.
findMacros :: [FilePath] -> Library -> IO (Maybe FilePath)
findMacros builddirs lib = listToMaybe . concat <$> mapM under builddirs
  where
    under builddir = do
      let build = builddir </> "build"
      platforms <- subdirs build
      compilers <- concat <$> mapM subdirs platforms
      concat <$> forM compilers (\c -> search (4 :: Int) (c </> libId lib))

    -- The main library is built directly under the package's directory, or
    -- one level down for a build without optimisation and the like. Other
    -- components are under a one-letter directory, such as @t@ for tests.
    search depth d = do
      let header = d </> "build" </> "autogen" </> "cabal_macros.h"
      here <- doesFileExist header
      if here
        then pure [header]
        else
          if depth == 0
            then pure []
            else concat <$> (mapM (search $ depth - 1) . filter ((> 1) . length . takeFileName) =<< subdirs d)

    subdirs d = do
      exists <- doesDirectoryExist d
      if exists
        then filterM doesDirectoryExist . map (d </>) =<< listDirectory d
        else pure []

-- | Mirror a project. Directories that lead to the first lot of
-- directories, or lead to or lie within the second lot, are made for real, the given files are left out, and everything
-- else is a link to the original. What is already there is left alone, but
-- a link where one of the left-out files goes is removed, so that writing
-- the file can't write through to the original.
--
-- Build directories are not mirrored, nor is the shadow itself when it is
-- inside the project. The result is the project's directories that were
-- made for real, whose contents a later run has to look at again.
mirror :: FilePath -> FilePath -> [FilePath] -> [FilePath] -> [FilePath] -> IO [FilePath]
mirror root out leadTo within leftOut = do
  rootAbs <- canonicalizePath root
  createDirectoryIfMissing True out
  outAbs <- canonicalizePath out
  let omit = map normalise leftOut
      inside = nub $ map normalise within
      leads r d = d == r || (r ++ "/") `isPrefixOf` d
      real r =
        any (leads r) (map normalise leadTo ++ inside)
          || any (\d -> d == "." || (d ++ "/") `isPrefixOf` r) inside
      go rel = do
        entries <- listDirectory (rootAbs </> rel)
        fmap ((normalise (rootAbs </> rel) :) . concat) . forM entries $ \e -> do
          let r = normalise $ rel </> e
              from = rootAbs </> r
              to = outAbs </> r
          isDir <- doesDirectoryExist from
          -- A build directory under any name: sharing one with the project
          -- would have each build undo the other's.
          isBuildDir <- doesFileExist $ from </> "cache" </> "plan.json"
          exists <- (||) <$> doesPathExist to <*> pathIsLink to
          if
            | "dist-newstyle" `isPrefixOf` e || isBuildDir || from == outAbs -> pure []
            | r `elem` omit -> do
                link <- pathIsLink to
                when link $ removeFile to
                pure []
            | isDir && e /= ".git" && real r -> do
                createDirectoryIfMissing True to
                go r
            | exists -> pure []
            | isDir -> [] <$ createDirectoryLink from to
            | otherwise -> [] <$ createFileLink from to
  go ""
  where
    pathIsLink p = do
      there <- doesPathExist p
      -- A dangling link doesn't exist as far as 'doesPathExist' can tell.
      if there then pathIsSymbolicLink p else pure False
