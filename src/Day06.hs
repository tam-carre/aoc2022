module Day06 (main) where

main ∷ IO ()
main = do
  input ← readFile "./inputs/Day06.txt"
  putStr $ strUnlines
    [ "Part 1:", show (lengthUntilNUniqChars 4 input)
    , "Part 2:", show (lengthUntilNUniqChars 14 input)
    ]

lengthUntilNUniqChars ∷ Int → String → Int
lengthUntilNUniqChars n = loop [] where
  loop seen (c:cs) | isNUniq seen = length seen
                   | otherwise    = loop (c:seen) cs
  isNUniq (take n → cs) = length cs ≡ n ∧ hasNoDupes cs
  hasNoDupes cs = length (ordNub cs) ≡ length cs
