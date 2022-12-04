module Day04 (day04A, day04B) where

import Data.List.Extra (intersect, isInfixOf, splitOn)
import Prelude         hiding (read)
import Relude.Unsafe   (read)

day04A ∷ IO Int
day04A = length . filter rangeFullyContainsOther <$> loadElfPairs where
  rangeFullyContainsOther (a,b) = a `isInfixOf` b ∨ b `isInfixOf` a

day04B ∷ IO Int
day04B = length . filter rangesOverlap <$> loadElfPairs where
  rangesOverlap (a,b) = not . null $ a `intersect` b

type Range = [Int]

loadElfPairs ∷ IO [(Range, Range)]
loadElfPairs = parseElfPairs <$> readFile "./inputs/Day04.txt" where
  parseElfPairs = map ((\[a,b] → (a,b)) . map parseRange . splitOn ",") . strLines
  parseRange    = (\[lo,hi] → [lo..hi]) . map read . splitOn "-"
