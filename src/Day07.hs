module Day07 where

import Data.List.Extra (dropEnd1, minimum, splitOn, stripPrefix)
import Data.Map        qualified as Map

main ∷ IO ()
main = do
  input ← readFile "./inputs/Day07.txt"
  putStr $ strUnlines
    [ "Part 1:", show . sumOfSizesUnder100k $ parseSizes input
    , "Part 2:", show . smallestDirToDelete $ parseSizes input
    ]

type Path = [String]

sumOfSizesUnder100k ∷ Map Path Int → Int
sumOfSizesUnder100k = sum . filter (≤ 100_000) . Map.elems

smallestDirToDelete ∷ Map Path Int → Int
smallestDirToDelete dirs = let
  (total, needed) = (70_000_000, 30_000_000)
  free            = total - (dirs Map.! ["/"])
  toDelete        = needed - free
  in minimum . filter (≥ toDelete) $ Map.elems dirs

parseSizes ∷ String → Map Path Int
parseSizes = snd . foldl' runLine ([], Map.empty) . strLines where
  runLine (cd, sizes) ln = case strWords ln of
    ["$", "cd", ".."]          → (dropEnd1 cd, sizes)
    ["$", "cd", dir]           → (cd ++ [dir], Map.alter (<|> Just 0) (cd ++ [dir]) sizes)
    [readMaybe → Just size, _] → (cd, Map.mapWithKey (incrWithParents cd size) sizes)
    _                          → (cd, sizes)
  incrWithParents cd sz path oldSz = if path `isPrefixOf` cd then oldSz + sz else oldSz
