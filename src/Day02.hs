{-# LANGUAGE OverloadedRecordDot #-}

module Day02 (day02A, day02B) where

import Data.Text qualified as T

----------------------------------------------------------------------------------------------------
-- Common

getInput ∷ IO Text
getInput = decodeUtf8 <$> readFileBS "./inputs/Day02AInput.txt"

data Move = Rock | Paper | Scissors deriving (Eq)

data MoveData
  = MoveData
    { score ∷ Int
    , beats ∷ Move
    , loses ∷ Move
    }

move ∷ Move → MoveData
move Rock     = MoveData 1 Scissors Paper
move Paper    = MoveData 2 Rock     Scissors
move Scissors = MoveData 3 Paper    Rock

abcToMove ∷ Text → Move
abcToMove "A" = Rock
abcToMove "B" = Paper
abcToMove "C" = Scissors
abcToMove _   = error "Invalid input"

data Outcome = Win | Draw | Loss deriving (Eq)

outcomeScore ∷ Outcome → Int
outcomeScore Win  = 6
outcomeScore Draw = 3
outcomeScore Loss = 0

----------------------------------------------------------------------------------------------------
-- Part A

-- | Total score according to strategy guide assuming ABC and XYZ are moves
day02A ∷ IO Int
day02A = sum . map roundScoreA <$> getRoundsA

roundScoreA ∷ (Move, Move) → Int
roundScoreA (elf, me) = (move me).score + outcomeScore out where
  out | (move me).beats ≡ elf = Win
      | me ≡ elf              = Draw
      | otherwise             = Loss

getRoundsA ∷ IO [(Move, Move)]
getRoundsA = parse <$> getInput where
  parse = map (listToRound . T.split (≡ ' ')) . lines
  listToRound [a,b] = (abcToMove a, xyzToMove b)
  listToRound _     = error "Invalid input"
  xyzToMove "X" = Rock
  xyzToMove "Y" = Paper
  xyzToMove "Z" = Scissors
  xyzToMove _   = error "Invalid input"

----------------------------------------------------------------------------------------------------
-- Part B

-- | Total score according to strategy guide assuming XYZ is desired outcome
day02B ∷ IO Int
day02B = sum . map roundScoreB <$> getRoundsB

roundScoreB ∷ (Move, Outcome) → Int
roundScoreB (elf, out) = (move me).score + outcomeScore out where
  me | out ≡ Win  = (move elf).loses
     | out ≡ Draw = elf
     | otherwise  = (move elf).beats

getRoundsB ∷ IO [(Move, Outcome)]
getRoundsB = parse <$> getInput where
  parse = map (listToRound . T.split (≡ ' ')) . lines
  listToRound [a,b] = (abcToMove a, xyzToOutcome b)
  listToRound _     = error "Invalid input"
  xyzToOutcome "X" = Loss
  xyzToOutcome "Y" = Draw
  xyzToOutcome "Z" = Win
  xyzToOutcome _   = error "Invalid input"
