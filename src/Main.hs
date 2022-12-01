module Main (main) where

import Day01 (day01A, day01B)

main ∷ IO ()
main = do
  echo "Day 1 answers:"
  print =≪ day01A
  print =≪ day01B
