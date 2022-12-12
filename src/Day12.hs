{-# LANGUAGE OverloadedRecordDot #-}

module Day12 where

import Control.Lens  (over, set)
import Data.Map      qualified as Map
import Prelude       hiding (head, init)
import Relude.Unsafe (head, init)

main ∷ IO ()
main = do
  climb ← parseClimb <$> readFile "./inputs/Day12.txt"
  putStr $ strUnlines
    [ "Part 1:", show . head . sort . map length $ pathCandidates climb
    , "Part 2:", "_"
    ]

data Climb = Climb
  { start ∷ (Int,Int)
  , goal  ∷ (Int,Int)
  , map   ∷ Map (Int,Int) Int
  } deriving (Generic, Show)

type Path = [(Int,Int)]

pathCandidates ∷ Climb → [Path]
pathCandidates climb = concatMap explore [[climb.start]] where
  explore path@((x,y):seen)
    | climb.goal ≡ (x,y) = [init path]
    | otherwise          = concatMap explore possibleNextPaths
    where
    possibleNextPaths =
      map (: path) $ mapMaybe stepIfViable [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]
    stepIfViable pos =
      Map.lookup pos climb.map ≫= \elevatn →
        if not (pos ∈ seen) ∧ abs (elevatn - curElevatn) < 2
          then Just pos
          else Nothing
    curElevatn = climb.map Map.! (x,y)

parseClimb ∷ String → Climb
parseClimb = snd . foldl' parseChar ((0,0), Climb (0,0) (0,0) Map.empty) where
  parseChar ((x,y), climb) = \case
    '\n' → ((0,y+1), climb)
    'S'  → ((x+1,y), climb & save 'a' & set #start (x,y))
    'E'  → ((x+1,y), climb & save 'z' & set #goal (x,y))
    c    → ((x+1,y), climb & save c)
    where save = over #map . Map.insert (x,y) . ord
