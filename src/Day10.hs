module Day10 where

import Data.List.Extra (chunksOf)
import Relude.Unsafe   (read, (!!))

main ∷ IO ()
main = do
  cycles ← parseRegisterHistory <$> readFile "./inputs/Day10.txt"
  putStr . strUnlines $
    [ "Part 1:", show $ sumOfSignalStrengths cycles
    , "Part 2:" ] ++ drawScreen cycles

sumOfSignalStrengths ∷ [Int] → Int
sumOfSignalStrengths history = foldr (\cy → (cy * (history !! cy) +)) 0 [20,60..220]

drawScreen ∷ [Int] → [String]
drawScreen history = chunksOf 40 $ map pixel [1..240] where
  pixel cy = if currentPx ∈ [sprite..sprite+2] then '#' else '.' where
    currentPx = cy `mod` 40
    sprite    = history !! cy

parseRegisterHistory ∷ String → [Int]
parseRegisterHistory = scanl (+) 1 . (0 :) . concatMap (parseLn . strWords) . strLines where
  parseLn ["noop"]      = [0]
  parseLn ["addx", num] = [0, read num]
