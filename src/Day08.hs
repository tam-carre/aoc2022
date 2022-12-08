module Day08 where

import Data.Char       (digitToInt)
import Data.List.Extra (maximum)
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
  takeWhileIncl p = (\(ok, rest) → ok ++ take 1 rest) . span p
  takeWhileEndIncl p = reverse . takeWhileIncl p . reverse
  score (x,y,el) = scoreLeft * scoreRight * scoreTop * scoreBottom where
    (line, col) = (mtx !! y, transpose mtx !! x)
    scoreTop    = length . takeWhileEndIncl (< el) . take y $ col
    scoreLeft   = length . takeWhileEndIncl (< el) . take x $ line
    scoreBottom = length . takeWhileIncl (< el) . drop (y+1) $ col
    scoreRight  = length . takeWhileIncl (< el) . drop (x+1) $ line

flat ∷ [[a]] → [(Int, Int, a)] -- [(xcord, ycord, a)]
flat = join . zipWith (\y → zipWith (,y,) [0..]) [0..]
