-- | The local packages of a project and their flag settings, read from
-- the @plan.json@ that cabal writes in its build directory.
module Shatter.Plan
  ( Local (..)
  , ProjectPlan (..)
  , readPlan
  ) where

import Control.Exception (IOException, try)
import Data.Char (chr, isDigit, isHexDigit, isSpace)
import Data.List (nubBy)
import Data.Maybe (mapMaybe)
import Numeric (readHex)

-- | A package whose source is in the project.
data Local = Local
  { localName :: String
  , localDir :: FilePath
  , localFlags :: [(String, Bool)]
  }

data ProjectPlan = ProjectPlan
  { planCompiler :: String
  -- ^ As cabal names it, such as @ghc-9.4.7@.
  , planLocals :: [Local]
  }

-- | The compiler and local packages in a plan file, or 'Nothing' if it
-- can't be read.
readPlan :: FilePath -> IO (Maybe ProjectPlan)
readPlan file = do
  text <- try (readFile file) :: IO (Either IOException String)
  pure $ case text of
    Left _ -> Nothing
    Right t -> do
      (plan, _) <- value t
      Array units <- field "install-plan" plan
      String compiler <- field "compiler-id" plan
      pure . ProjectPlan compiler . nubBy (\a b -> localDir a == localDir b) $ mapMaybe local units
  where
    local unit = do
      String "local" <- field "style" unit
      String name <- field "pkg-name" unit
      src <- field "pkg-src" unit
      String "local" <- field "type" src
      String dir <- field "path" src
      let flags = case field "flags" unit of
            Just (Object fs) -> [(f, on) | (f, Bool on) <- fs]
            _ -> []
      pure $ Local name dir flags

data Json
  = Object [(String, Json)]
  | Array [Json]
  | String String
  | Bool Bool
  | Other

field :: String -> Json -> Maybe Json
field k = \case
  Object fs -> lookup k fs
  _ -> Nothing

-- | A JSON value at the start of a text, and the rest of the text. Numbers
-- and null are recognised but not kept.
value :: String -> Maybe (Json, String)
value t = case dropWhile isSpace t of
  '{' : rest -> members [] rest
  '[' : rest -> elements [] rest
  '"' : rest -> (\(s, r) -> (String s, r)) <$> string "" rest
  't' : 'r' : 'u' : 'e' : rest -> Just (Bool True, rest)
  'f' : 'a' : 'l' : 's' : 'e' : rest -> Just (Bool False, rest)
  'n' : 'u' : 'l' : 'l' : rest -> Just (Other, rest)
  rest@(c : _) | isDigit c || c == '-' -> Just (Other, dropWhile (`elem` "0123456789+-.eE") rest)
  _ -> Nothing
  where
    members acc r = case dropWhile isSpace r of
      '}' : rest -> Just (Object $ reverse acc, rest)
      ',' : rest -> members acc rest
      '"' : rest -> do
        (k, r1) <- string "" rest
        ':' : r2 <- Just $ dropWhile isSpace r1
        (v, r3) <- value r2
        members ((k, v) : acc) r3
      _ -> Nothing
    elements acc r = case dropWhile isSpace r of
      ']' : rest -> Just (Array $ reverse acc, rest)
      ',' : rest -> elements acc rest
      rest -> do
        (v, r1) <- value rest
        elements (v : acc) r1
    string acc = \case
      '"' : rest -> Just (reverse acc, rest)
      '\\' : 'u' : a : b : c : d : rest
        | all isHexDigit [a, b, c, d]
        , [(n, "")] <- readHex [a, b, c, d] ->
            string (chr n : acc) rest
      '\\' : c : rest -> string (maybe c id (lookup c escapes) : acc) rest
      c : rest -> string (c : acc) rest
      [] -> Nothing
    escapes = [('n', '\n'), ('t', '\t'), ('r', '\r'), ('b', '\b'), ('f', '\f')]
