-- | Advice on dividing a package's modules so that it builds faster.
--
-- With enough cores, a build takes as long as its longest chain of modules
-- that each import the one before. The advice is the modules to divide to
-- shorten that chain, found by trying each division and keeping the best.
module Shatter.Advise
  ( Unit (..)
  , Piece (..)
  , Advice (..)
  , Step (..)
  , advise
  , chainOf
  , importsOf
  , showAdvice
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (intercalate, isPrefixOf, maximumBy, sortOn)
import Data.Map.Lazy qualified as Lazy
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set qualified as Set
import Text.PrettyPrint
import Prelude hiding ((<>))
import Text.Printf (printf)

-- | A module of the package.
data Unit = Unit
  { unitName :: String
  , unitTime :: Double
  -- ^ How long it takes to compile, in milliseconds.
  , unitLines :: Int
  , unitImports :: [String]
  -- ^ The modules it imports, as written.
  , unitPieces :: [Piece]
  -- ^ The pieces it could be divided into; none if it can't be.
  , unitRest :: [String]
  -- ^ With the whole package divided, what is imported by the module that
  -- is left under its name: its pieces and what its export list passes on.
  , unitGroups :: Int
  -- ^ How many groups of declarations that need each other it has; zero
  -- if it couldn't be read.
  , unitBinds :: [String]
  -- ^ The names it binds.
  }

-- | Part of a module: declarations that could be a module of their own.
data Piece = Piece
  { pieceName :: String
  , pieceTime :: Double
  , pieceLines :: Int
  , pieceBinds :: [String]
  , pieceImports :: [String]
  -- ^ The modules and pieces it would import, with the whole package
  -- divided.
  , pieceSingle :: Bool
  -- ^ Whether it is one group of declarations that need each other, and
  -- so can't be divided again.
  }

data Advice = Advice
  { adviceWork :: Double
  , adviceBefore :: (Double, [String])
  -- ^ The longest chain as the package stands, and the modules on it.
  , adviceSteps :: [Step]
  , adviceAfter :: (Double, [String])
  -- ^ The longest chain with every step taken; pieces are named.
  }

-- | One module to divide, and the longest chain once it is.
data Step = Step
  { stepUnit :: Unit
  , stepChain :: Double
  }

-- | The modules a source text imports, apart from source imports, which
-- don't make the importer wait.
importsOf :: String -> [String]
importsOf = mapMaybe one . lines
  where
    one l = case words l of
      "import" : rest
        | "SOURCE" `notElem` rest ->
            listToMaybe . filter (not . null) . map name $ dropWhile skip rest
      _ -> Nothing
    skip w = w `elem` ["qualified", "safe"] || "\"" `isPrefixOf` w
    name = takeWhile (\c -> isAlphaNum c || c `elem` "._'") . dropWhile isSpace

-- | The longest chain in a graph of what each node waits for, by total
-- time, and the nodes on it from first compiled to last.
chainOf :: Map String (Double, [String]) -> (Double, [String])
chainOf g
  | Map.null g = (0, [])
  | otherwise = (\(t, path) -> (t, reverse path)) . maximumBy (comparing fst) $ Lazy.elems finish
  where
    finish = Lazy.mapWithKey one g
    one n (w, deps) = case mapMaybe (`Lazy.lookup` finish) $ filter (/= n) deps of
      [] -> (w, [n])
      fs -> let (t, path) = maximumBy (comparing fst) fs in (w + t, n : path)

-- | The modules to divide, most useful first, and the longest chain after
-- each with those before it divided too. A limit above zero keeps only
-- that many.
--
-- Dividing a module assumes that its importers are changed to import only
-- the pieces they use.
advise :: Int -> [Unit] -> Advice
advise limit units =
  Advice
    { adviceWork = sum $ map unitTime units
    , adviceBefore = before
    , adviceSteps = steps
    , adviceAfter = chainOf . graph . Set.fromList $ map (unitName . stepUnit) steps
    }
  where
    byName = Map.fromList [(unitName u, u) | u <- units]
    home = Map.fromList $ [(unitName u, unitName u) | u <- units] ++ [(pieceName p, unitName u) | u <- units, p <- unitPieces u]

    -- As the package stands, a module waits for every module it imports.
    before = chainOf $ Map.fromList [(unitName u, (unitTime u, filter (`Map.member` byName) $ unitImports u)) | u <- units]

    -- With some modules divided, each of their pieces is a node, and an
    -- undivided module stands for all of its own.
    graph chosen = Map.fromListWith merge $ concatMap nodes units
      where
        merge (w, ds) (_, es) = (w, ds ++ es)
        owner x = do
          m <- Map.lookup x home
          pure $ if m `Set.member` chosen then x else m
        deps = mapMaybe owner
        nodes u
          | unitName u `Set.member` chosen =
              (unitName u, (0, deps $ unitRest u))
                : [(pieceName p, (pieceTime p, deps $ pieceImports p)) | p <- unitPieces u]
          | null (unitPieces u) = [(unitName u, (unitTime u, deps $ unitRest u))]
          | otherwise = [(unitName u, (unitTime u, deps $ unitRest u ++ concatMap pieceImports (unitPieces u)))]

    chain = fst . chainOf . graph

    -- Dividing one module often does nothing until the modules that import
    -- it are divided too, so the search runs backwards from everything
    -- divided. First a quick pass drops each module, cheapest first, whose
    -- division the chain doesn't need. Then the rest are dropped one at a
    -- time, each time the one the chain misses least; that order reversed
    -- is the order to divide them in.
    everything = Set.fromList [unitName u | u <- units, length (unitPieces u) > 1]
    best = chain everything
    needed = foldl drop' everything . sortOn unitTime $ filter ((`Set.member` everything) . unitName) units
    drop' chosen u
      | chain without <= best * 1.005 = without
      | otherwise = chosen
      where
        without = Set.delete (unitName u) chosen

    steps = (if limit > 0 then take limit else id) . reverse $ ranked needed
    ranked chosen
      | Set.null chosen = []
      | otherwise =
          let least = snd $ minimum [(chain $ Set.delete m chosen, m) | m <- Set.toList chosen]
           in [Step u (chain chosen) | Just u <- [Map.lookup least byName]] ++ ranked (Set.delete least chosen)

-- | The advice for one package, as text. The flag says whether the times
-- were measured; the name is the package's.
showAdvice :: String -> Bool -> [Unit] -> Advice -> String
showAdvice name measured units advice =
  render . vcat $
    [ text name
        <> colon
        <+> int (length units)
        <+> text "modules,"
        <+> secs (adviceWork advice)
        <+> text "of compile work"
        <+> parens (text $ if measured then "measured" else "estimated from size")
    , text "longest chain as it stands:" <+> secs (fst $ adviceBefore advice)
    , nest 2 . vcat . map plain . take 8 . sortOn (Down . unitTimeOf) $ snd (adviceBefore advice)
    , text ""
    ]
      ++ ( if null (adviceSteps advice)
            then [text "no division shortens the chain"]
            else
              [text "modules to divide, most useful first; each chain is with those above divided too:"]
                ++ zipWith3 step [1 :: Int ..] (fst (adviceBefore advice) : map stepChain (adviceSteps advice)) (adviceSteps advice)
                ++ [ text ""
                   , text "longest chain after these:" <+> secs (fst $ adviceAfter advice)
                   , nest 2 . vcat . map heavy . heaviest $ snd (adviceAfter advice)
                   ]
         )
  where
    byName = Map.fromList [(unitName u, u) | u <- units]
    pieces = Map.fromList [(pieceName p, (u, p)) | u <- units, p <- unitPieces u]
    divided = Set.fromList $ map (unitName . stepUnit) (adviceSteps advice)

    timeOf n = case (Map.lookup n pieces, Map.lookup n byName) of
      (Just (_, p), _) -> pieceTime p
      (_, Just u) | n `Set.notMember` divided || null (unitPieces u) -> unitTime u
      _ -> 0
    heaviest = take 8 . filter ((> 0) . timeOf) . sortOn (Down . timeOf)

    plain n = secs (unitTimeOf n) <+> text n
    unitTimeOf n = maybe 0 unitTime $ Map.lookup n byName
    heavy n = secs (timeOf n) <+> text (label n) <+> note n
    label n = case Map.lookup n pieces of
      Just (u, p) -> unitName u ++ ", piece with " ++ binds 3 (pieceBinds p)
      Nothing -> n
    note n = case (Map.lookup n pieces, Map.lookup n byName) of
      (Just (_, p), _)
        | pieceSingle p -> text "(one group of declarations, can't be divided)"
        | otherwise -> text "(could be divided again)"
      (_, Just u)
        | unitGroups u == 1 -> text "(one group of declarations:" <+> text (binds 3 $ unitBinds u) <> text ")"
        | unitGroups u == 0 -> text "(not read, so not considered)"
        | null (unitPieces u) -> empty
        | n `Set.member` divided -> empty
        | otherwise -> text "(dividing it doesn't shorten the chain)"
      _ -> empty

    step i from (Step u to) =
      vcat
        [ nest 2 $
            int i
              <> text "."
              <+> text (unitName u)
              <> colon
              <+> text "chain"
              <+> secs from
              <+> text "->"
              <+> secs to
        , nest 5 . vcat $ zipWith (piece u) [1 :: Int ..] (unitPieces u)
        ]
    piece u i p =
      hang
        ( text "piece"
            <+> int i
            <> colon
            <+> int (pieceLines p)
            <+> text "lines,"
            <+> secs (pieceTime p)
            <> needs u p
        )
        2
        (text $ binds 6 (pieceBinds p))
    needs u p = case [j | (j, q) <- zip [1 :: Int ..] (unitPieces u), pieceName q `elem` pieceImports p] of
      [] -> empty
      js -> text ", needs" <+> text (intercalate ", " $ map show js)

    binds n names = case splitAt n names of
      (shown, []) -> intercalate ", " shown
      (shown, more) -> intercalate ", " shown ++ " and " ++ show (length more) ++ " more"

    secs :: Double -> Doc
    secs ms = text (printf "%.2f s" (ms / 1000))
