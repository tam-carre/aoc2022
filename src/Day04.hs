module Day04 (day04A, day04B) where

import Data.List.Extra (intersect, isInfixOf, splitOn)
import Prelude         hiding (read)
import Relude.Unsafe   (read)

day04A ∷ IO Int
day04A = length . filter rangeFullyContainsOther <$> loadElfPairs where
  rangeFullyContainsOther (a,b)  = a `contains` b ∨ b `contains` a
  contains (loA, hiA) (loB, hiB) = [loB..hiB] `isInfixOf` [loA..hiA]

day04B ∷ IO Int
day04B = length . filter rangesOverlap <$> loadElfPairs where
  rangesOverlap (a,b) = a `overlaps` b
  overlaps (loA, hiA) (loB, hiB) = not . null $ [loA..hiA] `intersect` [loB..hiB]

type Range = (Int, Int)

loadElfPairs ∷ IO [(Range, Range)]
loadElfPairs = parseElfPairs <$> readFile "./inputs/Day04.txt" where
  parseElfPairs   = map (parsePair "," . parsePair "-" $ read) . strLines
  parsePair sep f = (\[a,b] → (a,b)) . map f . splitOn sep
