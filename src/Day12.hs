{-# LANGUAGE OverloadedRecordDot #-}

module Day12 where

import Control.Lens   (over, set)
import Data.Map       qualified as Map
import Data.Set.Monad (Set)
import Data.Set.Monad qualified as Set
import Prelude        hiding (head, init, Set)
import Relude.Unsafe  (head, init)

main ∷ IO ()
main = do
  climb ← parseClimb <$> readFile "./inputs/Day12.txt"
  putStr $ strUnlines
    [ "Part 1:", show . head . sort . map length . Set.elems $ pathCandidates climb
    , "Part 2:", "_"
    ]

data Climb = Climb
  { start ∷ (Int,Int)
  , goal  ∷ (Int,Int)
  , map   ∷ Map (Int,Int) Int
  } deriving (Generic, Show)

type Path = Set (Int,Int)

pathCandidates ∷ Climb → Set Path
pathCandidates climb = Set.singleton Set.empty ≫= explore climb.start where
  explore ∷ (Int,Int) → Path → Set Path
  explore (x,y) path
    | climb.goal ≡ (x,y) = Set.singleton path
    | otherwise          = possibleNextPaths ≫= uncurry explore
    where
    possibleNextPaths
      = fmap (, Set.insert (x,y) path)
      . Set.fromList
      . mapMaybe stepIfViable
      $ [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]
    stepIfViable pos =
      Map.lookup pos climb.map ≫= \elevatn →
        if not (pos `Set.member` path) ∧ abs (elevatn - curElevatn) < 2
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
