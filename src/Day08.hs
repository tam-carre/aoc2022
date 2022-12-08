module Day08 where

import Data.Char       (digitToInt)
import Data.List.Extra (maximum, takeWhileEnd)
import Relude.Unsafe   ((!!))

main ∷ IO ()
main = do
  trees ← map (map digitToInt) . strLines <$> readFile "./inputs/Day08.txt"
  putStr $ strUnlines
    [ "Part 1:", show . numberVisibleFromOutside $ trees
    , "Part 2:", show . highestScenicScore $ trees
    ]

numberVisibleFromOutside ∷ [[Int]] → Int
numberVisibleFromOutside mtx = length . filter visible $ flat mtx where
  visible (x,y,el) = visLeft ∨ visRight ∨ visTop ∨ visBottom where
    (line, col) = (mtx !! y, transpose mtx !! x)
    visTop      = all (< el) . take y     $ col
    visLeft     = all (< el) . take x     $ line
    visBottom   = all (< el) . drop (y+1) $ col
    visRight    = all (< el) . drop (x+1) $ line

highestScenicScore ∷ [[Int]] → Int
highestScenicScore mtx = maximum . map score $ flat mtx where
  width  = length $ mtx !! 0
  height = length mtx
  score (x,y,el) = scoreLeft * scoreRight * scoreTop * scoreBottom where
    (line, col) = (mtx !! y, transpose mtx !! x)
    scoreTop    = min y . (1 +) . length . takeWhileEnd (< el) . take y $ col
    scoreLeft   = min x . (1 +) . length . takeWhileEnd (< el) . take x $ line
    scoreBottom = min (height-y-1) . (1 +) . length . takeWhile (< el) . drop (y+1) $ col
    scoreRight  = min (width-x-1) . (1 +) . length . takeWhile (< el) . drop (x+1) $ line

-- | flattens an [[a]] matrix into an [(xCord, yCord, a)]
flat ∷ [[a]] → [(Int, Int, a)]
flat = join . zipWith (\y → zipWith (,y,) [0..]) [0..]
