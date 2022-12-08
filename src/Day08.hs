module Day08 where

import Data.Char       (digitToInt)
import Data.List.Extra (takeWhileEnd)
import GHC.Utils.Misc  (applyWhen)
import Relude.Unsafe   ((!!))

main ∷ IO ()
main = do
  trees ← map (map digitToInt) . strLines <$> readFile "./inputs/Day08.txt"
  putStr $ strUnlines
    [ "Part 1:", show . numberVisibleFromOutside $ trees
    , "Part 2:", show . highestScenicScore $ trees
    ]

numberVisibleFromOutside ∷ [[Int]] → Int
numberVisibleFromOutside mtx = loop (0,0,0) where
  loop (count,x,y) | y ≥ height = count
                   | x ≥ width  = loop (count, 0, y + 1)
                   | otherwise  = loop (applyWhen (visible x y) (1 +) count, x + 1, y)
  width  = length $ mtx !! 0
  height = length mtx
  visible x y = visLeft ∨ visRight ∨ visTop ∨ visBottom where
    vis = all (< mtx !! y !! x)
    visLeft   = vis . take x $ mtx !! y
    visRight  = vis . drop (x+1) $ mtx !! y
    visTop    = vis . take y $ transpose mtx !! x
    visBottom = vis . drop (y+1) $ transpose mtx !! x

highestScenicScore ∷ [[Int]] → Int
highestScenicScore mtx = loop (0,0,0) where
  loop (highest,x,y) | y ≥ height = highest
                     | x ≥ width  = loop (highest, 0, y + 1)
                     | otherwise  = loop (max (score x y) highest, x + 1, y)
  width  = length $ mtx !! 0
  height = length mtx
  score x y = scoreLeft * scoreRight * scoreTop * scoreBottom where
    el = mtx !! y !! x
    scoreLeft   = min x . (1 +) . length . takeWhileEnd (< el) . take x $ mtx !! y
    scoreRight  = min (width-x-1) . (1 +) . length . takeWhile (< el) . drop (x+1) $ mtx !! y
    scoreTop    = min y . (1 +) . length . takeWhileEnd (< el) . take y $ transpose mtx !! x
    scoreBottom = min (height-y-1) . (1 +) . length . takeWhile (< el) . drop (y+1) $ transpose mtx !! x
