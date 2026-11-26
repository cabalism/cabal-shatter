-- | Parse one Haskell source file with the GHC parser, without renaming or
-- typechecking it.
module Shatter.Parse
  ( Parsed (..)
  , Cpp (..)
  , parseFile
  , lineSpan
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace, toLower)
import Control.Exception (evaluate)
import Data.Char (isDigit)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (intercalate, isInfixOf, isPrefixOf, unsnoc)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import GHC
  ( GhcPs
  , HsModule (..)
  , LHsDecl
  , LIE
  , LImportDecl
  , getLocA
  , getSessionDynFlags
  , moduleNameString
  , noLoc
  , parseDynamicFlags
  , runGhc
  , unLoc
  )
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Utils.Logger (getLogger)
import GHC.Parser (parseModule)
import GHC.Parser.Lexer (P (unP), ParseResult (..), initParserState)
import GHC.Types.SrcLoc (SrcSpan (..), mkRealSrcLoc, srcSpanEndLine, srcSpanStartLine)
import GHC.Settings.Config (cProjectVersion)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Process (readProcess, readProcessWithExitCode)

-- | What the splitter needs from a parsed module.
data Parsed = Parsed
  { parsedName :: String
  -- ^ The module's name, or @Main@ when it has no header.
  , parsedDecls :: [LHsDecl GhcPs]
  -- ^ The top-level declarations, in source order.
  , parsedLines :: Int
  -- ^ The number of lines in the file.
  , parsedImports :: [LImportDecl GhcPs]
  -- ^ The imports, in source order.
  , parsedSource :: [String]
  -- ^ The file's lines.
  , parsedNameLine :: Maybe Int
  -- ^ The line the module's name is on, when it has a header.
  , parsedExports :: Maybe [LIE GhcPs]
  -- ^ The export list, when the header has one.
  , parsedHasExports :: Bool
  -- ^ Whether the header has an export list.
  , parsedImportLines :: Maybe (Int, Int)
  -- ^ The first and last lines of the imports.
  }

-- | What the C preprocessor needs for a module that uses CPP.
data Cpp = Cpp
  { cppMacros :: [FilePath]
  -- ^ Header files to include first, such as cabal's @cabal_macros.h@.
  , cppFlags :: [String]
  -- ^ @-D@ and @-I@ options, as the package's @cpp-options@ and
  -- @include-dirs@ give them.
  }

-- | Parse a file. The extra flags are passed to GHC as if on the command
-- line, for extensions that the package turns on outside the file, such as
-- @-XHaskell2010@.
--
-- The GHC library needs the compiler's library directory, so this runs
-- @ghc --print-libdir@ with the given compiler, or with the compiler this
-- program was built against when none is given.
--
-- A module that uses CPP is preprocessed first, and what is parsed and
-- split is the preprocessor's output: the text GHC would have compiled
-- with these macros, with each line still on the line it came from.
parseFile :: Maybe FilePath -> Cpp -> [String] -> FilePath -> IO (Either String Parsed)
parseFile withGhc cpp extraFlags path = do
  original <- readFile path
  let ghc = maybe ("ghc-" ++ cProjectVersion) id withGhc
      usesCpp = any ((== "cpp") . map toLower) (languagePragmas original) || "-XCPP" `elem` extraFlags
  preprocessed <- if usesCpp then preprocess ghc cpp path else pure $ Right original
  case preprocessed of
    Left err -> pure $ Left err
    Right src -> do
      let pragmas = languagePragmas src
      libdir <- libdirOf ghc
      runGhc (Just libdir) $ do
        dflags0 <- getSessionDynFlags
        logger <- getLogger
        let flags = map noLoc $ filter (/= "-XCPP") extraFlags ++ map ("-X" ++) pragmas
        (dflags, _, _) <- parseDynamicFlags logger dflags0 flags
        let buf = stringToStringBuffer src
            loc = mkRealSrcLoc (mkFastString path) 1 1
            state = initParserState (initParserOpts dflags) buf loc
        liftIO . pure $ case unP parseModule state of
          PFailed _ -> Left $ path ++ ": does not parse"
          POk _ lm ->
            let m = unLoc lm
             in Right
                  Parsed
                    { parsedName = maybe "Main" (moduleNameString . unLoc) $ hsmodName m
                    , parsedDecls = hsmodDecls m
                    , parsedLines = length $ lines src
                    , parsedImports = hsmodImports m
                    , parsedSource = lines src
                    , parsedNameLine = fst <$> (lineSpan . getLocA =<< hsmodName m)
                    , parsedExports = unLoc <$> hsmodExports m
                    , parsedHasExports = isJust $ hsmodExports m
                    , parsedImportLines = case mapMaybe (lineSpan . getLocA) $ hsmodImports m of
                        [] -> Nothing
                        spans -> Just (minimum $ map fst spans, maximum $ map snd spans)
                    }

-- | The compiler's library directory, asked for once per compiler.
libdirOf :: FilePath -> IO FilePath
libdirOf ghc = do
  known <- readIORef libdirs
  case lookup ghc known of
    Just dir -> pure dir
    Nothing -> do
      dir <- trim <$> readProcess ghc ["--print-libdir"] ""
      modifyIORef libdirs ((ghc, dir) :)
      pure dir

libdirs :: IORef [(FilePath, FilePath)]
libdirs = unsafePerformIO $ newIORef []
{-# NOINLINE libdirs #-}

-- | Run GHC's preprocessing phase on a file and give back its text with
-- CPP turned off.
preprocess :: FilePath -> Cpp -> FilePath -> IO (Either String String)
preprocess ghc cpp path = do
  tmp <- getTemporaryDirectory
  (out, h) <- openTempFile tmp "shatter.hspp"
  hClose h
  -- Without -hide-all-packages GHC defines version macros for the
  -- packages it can see, which are not the ones the package is built with.
  let args =
        ["-E", "-cpp", "-v0", "-hide-all-packages", path, "-o", out]
          ++ concat [["-optP-include", "-optP" ++ m] | m <- cppMacros cpp]
          ++ cppFlags cpp
  (code, _, err) <- readProcessWithExitCode ghc args ""
  text <- readFile out
  _ <- evaluate $ length text
  removeFile out
  pure $ case code of
    ExitSuccess -> Right . unlines . map withoutCpp . realign path $ lines text
    ExitFailure _ -> Left $ path ++ ": the preprocessor failed\n" ++ err

-- | Put each line of the preprocessor's output back on the line of the
-- file it came from, going by the line markers, and drop the markers and
-- the lines that came from other files.
realign :: FilePath -> [String] -> [String]
realign path = fill 1 . Map.toAscList . go Nothing Map.empty
  where
    go _ acc [] = acc
    go at acc (l : ls) = case marker l of
      Just (n, file) -> go (if file == path then Just n else Nothing) acc ls
      Nothing -> case at of
        Just n -> go (Just $ n + 1) (Map.insert n l acc) ls
        Nothing -> go Nothing acc ls

    fill _ [] = []
    fill n rest@((m, l) : more)
      | n < m = "" : fill (n + 1) rest
      | otherwise = l : fill (n + 1) more

    -- @# 12 "File.hs"@ from the preprocessor, or the @LINE@ pragma GHC
    -- puts on the first line.
    marker :: String -> Maybe (Int, FilePath)
    marker l = case words l of
      "#" : n : file : _ | all isDigit n, Just f <- quoted file -> Just (read n, f)
      "{-#" : "LINE" : n : file : _ | all isDigit n, Just f <- quoted file -> Just (read n, f)
      _ -> Nothing
    quoted = \case
      '"' : rest | Just (f, '"') <- unsnoc rest -> Just f
      _ -> Nothing

-- | Take CPP out of a one-line @LANGUAGE@ pragma, leaving the line in place.
withoutCpp :: String -> String
withoutCpp l
  | "{-# language" `isPrefixOf` map toLower l
  , names <- languagePragmas l
  , any isCpp names =
      case filter (not . isCpp) names of
        [] -> ""
        rest -> "{-# LANGUAGE " ++ intercalate ", " rest ++ " #-}"
  | otherwise = l
  where
    isCpp = (== "cpp") . map toLower

-- | The first and last lines of a source span.
lineSpan :: SrcSpan -> Maybe (Int, Int)
lineSpan = \case
  RealSrcSpan s _ -> Just (srcSpanStartLine s, srcSpanEndLine s)
  _ -> Nothing

-- | The extensions named in @LANGUAGE@ pragmas above the module header.
languagePragmas :: String -> [String]
languagePragmas = concatMap names . takeWhile (not . isHeader) . lines
  where
    isHeader l = "module " `isPrefixOf` l || "import " `isPrefixOf` l
    names l
      | "{-# language" `isInfixOf` map toLower l =
          filter (not . null)
            . map trim
            . splitOn ','
            . takeWhile (/= '#')
            . drop (length "{-# language")
            $ dropWhile isSpace l
      | otherwise = []

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest

trim :: String -> String
trim = dropWhileEnd' isSpace . dropWhile isSpace
  where
    dropWhileEnd' p = reverse . dropWhile p . reverse
