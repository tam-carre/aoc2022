module Day03 (day03A, day03B) where

import Data.Char       (isLower, isUpper)
import Data.List.Extra (chunksOf)
import Data.Maybe      (fromJust)

-- | Sum of priority value of all items appearing in a rucksack's two cptments
day03A ∷ IO Int
day03A = sum . map (priorityOf . isInBoth . cptments) <$> loadSacks where
  cptments sack  = splitAt (length sack `div` 2) sack
  isInBoth (a,b) = fromJust $ find (∈ b) a

-- | Sum of priority value of all badges (common item in groups of 3 elves)
day03B ∷ IO Int
day03B = sum . map (priorityOf . badge) . elfGrps <$> loadSacks where
  elfGrps       = chunksOf 3
  badge [a,b,c] = fromJust $ find (\itm → itm ∈ b ∧ itm ∈ c) a

loadSacks ∷ IO [String]
loadSacks = strLines <$> readFile "./inputs/Day03AInput.txt"

priorityOf ∷ Char → Int
priorityOf c = ord c - baseline where
  baseline | isLower c = ord 'a' - 1
           | isUpper c = ord 'A' - 27
