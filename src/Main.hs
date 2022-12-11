module Main where

import Control.Lens (Getting, preview, view)
import Day01        (day01A, day01B)
import Day02        (day02A, day02B)
import Day03        (day03A, day03B)
import Day04        (day04A, day04B)
import Day05 qualified
import Day06 qualified
import Day07 qualified
import Day08 qualified
import Day09 qualified
import Day10 qualified
import Day11 qualified

main ∷ IO ()
main = do
  echo "Day 1 answers:"
  print =≪ day01A
  print =≪ day01B
  echo "Day 2 answers:"
  print =≪ day02A
  print =≪ day02B
  echo "Day 3 answers:"
  print =≪ day03A
  print =≪ day03B
  echo "Day 4 answers:"
  print =≪ day04A
  print =≪ day04B
  echo "Day 5 answers:"
  Day05.main
  echo "Day 6 answers:"
  Day06.main
  echo "Day 7 answers:"
  Day07.main
  echo "Day 8 answers:"
  Day08.main
  echo "Day 9 answers:"
  Day09.main
  echo "Day 10 answers:"
  Day10.main
  echo "Day 11 answers:"
  Day11.main
