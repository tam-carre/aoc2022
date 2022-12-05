module Day05 (main) where

import Data.Char       (isDigit)
import Data.List.Extra (splitAtEnd, trimStart, wordsBy)
import Data.Map        qualified as Map
import Prelude         hiding (head, tail)
import Relude.Unsafe   (head, read, tail)

main ∷ IO ()
main = do
  input ← readFile "./inputs/Day05.txt"
  putStr $ strUnlines
    [ "Part 1", pileTops . runMovesOneByOne $ parsePilesAndMoves input
    , "Part 2", pileTops . runMovesGrouped  $ parsePilesAndMoves input
    ]

type Pile = [Char]
data Move = Move { qty ∷ Int, src ∷ Int, to  ∷ Int }

pileTops ∷ Map Int Pile → [Char]
pileTops = map head . Map.elems

runMovesOneByOne ∷ (Map Int Pile, [Move]) → Map Int Pile
runMovesOneByOne = fst . loop where
  loop (piles, [])                 = (piles, [])
  loop (piles, (Move 0 _ _):xs)    = loop (piles, xs)
  loop (piles, (Move n src to):xs) = loop (moveOne src to piles, Move (n-1) src to : xs)
  moveOne src to piles = Map.adjust tail src $ Map.adjust (srcHead :) to piles where
    srcHead = head $ piles Map.! src

runMovesGrouped ∷ (Map Int Pile, [Move]) → Map Int Pile
runMovesGrouped = fst . loop where
  loop (piles, [])                 = (piles, [])
  loop (piles, (Move n src to):xs) = loop (moveN n src to piles, xs)
  moveN n src to piles = Map.adjust (drop n) src $ Map.adjust (srcTopN ++) to piles where
    srcTopN = take n $ piles Map.! src

parsePilesAndMoves ∷ String → (Map Int Pile, [Move])
parsePilesAndMoves = piles &&& moves where
  piles = Map.fromList
        . map (first read . swap . splitAtEnd 1 . trimStart) -- "  ABC1" → "(1, "AGC")"
        . filter (any isDigit) -- only keep the lines with pile number and contents
        . transpose -- turn the text 90 degrees
        . takeWhile (not . null) -- only keep the schema
        . strLines
  moves = map ((\[qty,src,to] → Move {..}) . map read . wordsBy (not . isDigit))
        . dropWhile (not . ("move " `isPrefixOf`))
        . strLines
