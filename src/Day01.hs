module Day01 (day01A, day01B) where

import Data.List (maximum)
import Data.List.Extra (takeEnd)
import Data.Text qualified as T

day01A ∷ IO Int
day01A = maximum . toIndividualCalCounts <$> getInput

day01B ∷ IO Int
day01B = sum . takeEnd 3 . sort . toIndividualCalCounts <$> getInput

getInput ∷ IO Text
getInput = decodeUtf8 <$> readFileBS "./src/Day01AInput.txt"

toIndividualCalCounts ∷ Text → [Int]
toIndividualCalCounts = map sumCals . T.splitOn "\n\n" where
  sumCals = sum . mapMaybe (readMaybe @Int . toString) . T.splitOn "\n"
