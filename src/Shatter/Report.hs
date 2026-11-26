-- | The report on how a module would split.
module Shatter.Report
  ( report
  ) where

import Data.List (intercalate)
import Shatter.Graph (Group (..), Kind (..), Node (..), declGroups, declNodes, pack)
import Shatter.Parse (Parsed (..))
import Text.PrettyPrint
  ( Doc
  , hsep
  , int
  , nest
  , parens
  , render
  , text
  , vcat
  , ($$)
  , (<+>)
  )
import Text.Printf (printf)

-- | Describe the groups a module would split into. The first argument is
-- how many of the largest groups to list; 'Nothing' lists them all. The
-- second is the least number of lines to pack into a fragment.
report :: Maybe Int -> Int -> Parsed -> String
report top packed parsed = render $ vcat [summary, text "", sizes, text "", largest, problems]
  where
    nodes = declNodes (parsedName parsed) (parsedDecls parsed)
    groups = pack packed $ declGroups nodes
    declLines = sum $ map groupLines groups
    share n = percent n declLines

    summary =
      vcat
        [ text "module:" <+> text (parsedName parsed)
        , text "lines:" <+> int (parsedLines parsed) <+> parens (int declLines <+> text "in" <+> int (length nodes) <+> text "declarations")
        , text "groups:" <+> int (length groups)
        , text "largest group:" <+> maybe (text "none") (\g -> lineCount (groupLines g) <+> parens (text . share $ groupLines g)) (headMay groups)
        , text "expected recompile for a one-line edit:" <+> text (printf "%.0f" expected) <+> text "lines" <+> parens (text $ percentD expected declLines)
        ]

    -- An edit recompiles the group it falls in, and falls in a group in
    -- proportion to the group's size.
    expected :: Double
    expected
      | declLines == 0 = 0
      | otherwise = sum [fromIntegral (groupLines g) ^ (2 :: Int) | g <- groups] / fromIntegral declLines

    sizes =
      text "groups by size:"
        $$ nest 2 (vcat [bucket "1-10 lines" 1 10, bucket "11-50 lines" 11 50, bucket "51-200 lines" 51 200, bucket "over 200 lines" 201 maxBound])

    bucket label lo hi =
      let inside = [g | g <- groups, groupLines g >= lo, groupLines g <= hi]
          total = sum $ map groupLines inside
       in hsep [text (label ++ ":"), int (length inside), text "groups,", lineCount total, parens . text $ share total]

    largest =
      text (maybe "all groups:" (\n -> "largest " ++ show n ++ " groups:") top)
        $$ nest 2 (vcat . map groupLine $ maybe id take top groups)

    groupLine g =
      hsep
        [ lineCount $ groupLines g
        , parens (text . share $ groupLines g) <> text ","
        , int . length $ groupNodes g
        , text "declarations:"
        , text $ names g
        ]

    names g = case groupNames g of
      [] -> "(binds no names, line " ++ intercalate ", " (show . nodeFirstLine <$> groupNodes g) ++ ")"
      ns
        | length ns > 8 -> intercalate ", " (take 8 ns) ++ ", and " ++ show (length ns - 8) ++ " more"
        | otherwise -> intercalate ", " ns

    problems =
      vcat $
        [text "" | not (null orphans && null splices)]
          ++ [text "orphan instances:" <+> int (length orphans) <+> parens (text "lines" <+> text (linesOf orphans)) | not (null orphans)]
          ++ [text "top-level splices, which the splitter refuses:" <+> int (length splices) <+> parens (text "lines" <+> text (linesOf splices)) | not (null splices)]

    orphans = filter nodeOrphan nodes
    splices = filter ((== Splice) . nodeKind) nodes
    linesOf = intercalate ", " . map (show . nodeFirstLine)

lineCount :: Int -> Doc
lineCount n = int n <+> text "lines"

percent :: Int -> Int -> String
percent n = percentD (fromIntegral n)

percentD :: Double -> Int -> String
percentD _ 0 = "0%"
percentD n total = printf "%.1f%%" (100 * n / fromIntegral total)

headMay :: [a] -> Maybe a
headMay = foldr (const . Just) Nothing
