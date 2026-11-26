-- | Parse one Haskell source file with the GHC parser, without renaming or
-- typechecking it.
module Shatter.Parse
  ( Parsed (..)
  , parseFile
  , lineSpan
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace, toLower)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, mapMaybe)
import GHC
  ( GhcPs
  , HsModule (..)
  , LHsDecl
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
import System.Process (readProcess)

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
  , parsedHasExports :: Bool
  -- ^ Whether the header has an export list.
  , parsedImportLines :: Maybe (Int, Int)
  -- ^ The first and last lines of the imports.
  }

-- | Parse a file. The extra flags are passed to GHC as if on the command
-- line, for extensions that the package turns on outside the file, such as
-- @-XHaskell2010@.
--
-- The GHC library needs the compiler's library directory, so this runs
-- @ghc --print-libdir@ with the given compiler, or with the compiler this
-- program was built against when none is given.
parseFile :: Maybe FilePath -> [String] -> FilePath -> IO (Either String Parsed)
parseFile withGhc extraFlags path = do
  src <- readFile path
  let pragmas = languagePragmas src
  if any ((== "cpp") . map toLower) pragmas
    then pure . Left $ path ++ ": uses CPP, which the splitter does not handle"
    else do
      let ghc = maybe ("ghc-" ++ cProjectVersion) id withGhc
      libdir <- trim <$> readProcess ghc ["--print-libdir"] ""
      runGhc (Just libdir) $ do
        dflags0 <- getSessionDynFlags
        logger <- getLogger
        let flags = map noLoc $ extraFlags ++ map ("-X" ++) pragmas
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
                    , parsedHasExports = isJust $ hsmodExports m
                    , parsedImportLines = case mapMaybe (lineSpan . getLocA) $ hsmodImports m of
                        [] -> Nothing
                        spans -> Just (minimum $ map fst spans, maximum $ map snd spans)
                    }

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
