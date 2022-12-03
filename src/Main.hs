module Main (main) where

import Day01 (day01A, day01B)
import Day02 (day02A, day02B)
import Day03 (day03A, day03B)

main ∷ IO ()
main = do
  echo "Day 1 answers:"
  print =≪ day01A
  print =≪ day01B
  echo "Day 2 answers:"
  print =≪ day02A
  print =≪ day02B
  echo "Day 2 answers:"
  print =≪ day03A
  print =≪ day03B
