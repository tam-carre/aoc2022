module Main (main) where

import Day01 (day01A, day01B)
import Day02 (day02A, day02B)

main ∷ IO ()
main = do
  echo "Day 1 answers:"
  print =≪ day01A
  print =≪ day01B
  echo "Day 2 answers:"
  print =≪ day02A
  print =≪ day02B
