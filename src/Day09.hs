{-# LANGUAGE OverloadedRecordDot #-}

module Day09 where

import Prelude       hiding (head, last)
import Relude.Unsafe (last, read)

main ∷ IO ()
main = do
  moves ← parseMoves <$> readFile "./inputs/Day09.txt"
  putStr $ strUnlines
    [ "Part 1:", show . totalVisitedPositionsByTail 1 $ moves -- 62336
    , "Part 2:", show . totalVisitedPositionsByTail 9 $ moves -- 2449
    ]

type Move = Char
data Pos  = Pos { x ∷ Int, y ∷ Int } deriving (Eq, Ord)
data Rope = Rope { head ∷ Pos, tail ∷ [Pos] } deriving (Eq, Ord)

totalVisitedPositionsByTail ∷ Int → [Move] → Int
totalVisitedPositionsByTail len = length . ordNub . visitedByTail . scanl runMove (mkRope len) where
  visitedByTail = map (last . (.tail))
  mkRope len    = Rope (Pos 0 0) . replicate len $ Pos 0 0
  runMove (Rope oldHead oldTail) move = Rope newHead (newTail newHead) where
    run movement (Pos x y) = case movement of
      'R' → Pos (x + 1) y
      'L' → Pos (x - 1) y
      'U' → Pos x       (y + 1)
      'D' → Pos x       (y - 1)
    newHead = run move oldHead
    newTail = loop oldTail where
      loop [] _ = []
      loop (tailHead:tailTail) head = let
        xΔ = head.x - tailHead.x
        yΔ = head.y - tailHead.y
        absXyΔ = abs xΔ + abs yΔ
        newTailHead = tailHead & when' (any (> 1) [abs xΔ, abs yΔ])
          ( when' (yΔ ≡ 2 ∨ (yΔ ≡ 1 ∧ absXyΔ ≡ 3))       (run 'U')
          . when' (xΔ ≡ 2 ∨ (xΔ ≡ 1 ∧ absXyΔ ≡ 3))       (run 'R')
          . when' (yΔ ≡ (-2) ∨ (yΔ ≡ (-1) ∧ absXyΔ ≡ 3)) (run 'D')
          . when' (xΔ ≡ (-2) ∨ (xΔ ≡ (-1) ∧ absXyΔ ≡ 3)) (run 'L')
          )
        in newTailHead:loop tailTail newTailHead

parseMoves ∷ String → [Move]
parseMoves = concatMap (\(dir:' ':n) → replicate (read n) dir) . strLines

when' ∷ Bool → (a → a) → a → a
when' p f a = if p then f a else a
