module Day11 where

import Control.Lens    (ix, over, set, view)
import Data.List.Extra (drop1, splitOn, takeEnd)
import Relude.Unsafe   (read, (!!))

main ∷ IO ()
main = do
  apes ← parseApes <$> readFile "./inputs/Day11.txt"
  putStr $ strUnlines
    [ "Part 1:", show . monkeyBusinessLv $ runRounds 20 Relaxed apes
    , "Part 2:", show . monkeyBusinessLv $ runRounds 10000 Anxious apes
    ]

data Ape
  = Ape { id ∷ Int, items ∷ [Int], op ∷ Int → Int, inspected ∷ Int, test ∷ Test }
  deriving (Generic)

data Test = Test { divBy ∷ Int, onTrue ∷ Int, onFalse ∷ Int } deriving (Generic)

data Anxiety = Relaxed | Anxious deriving (Eq)

monkeyBusinessLv ∷ [Ape] → Int
monkeyBusinessLv = product . takeEnd 2 . sort . map (view #inspected)

runRounds ∷ Int → Anxiety → [Ape] → [Ape]
runRounds howMany anxiety = iterate runRound ⋙ (!! howMany) where
  runRound apes  = foldl' runApe apes $ map (view #id) apes
  runApe apes id = foldl' (runItem id) apes $ view #items (apes !! id)
  runItem senderId apes itemWorryLv = apes
    & over (ix senderId . #items)     drop1
    & over (ix senderId . #inspected) (+1)
    & over (ix recipientId . #items)  (++ [itemNewWorryLv])
    where
    Ape { op, test } = apes !! senderId
    Test { divBy, onTrue, onFalse } = test
    recipientId = if itemNewWorryLv `mod` divBy ≡ 0 then onTrue else onFalse
    itemNewWorryLv = op itemWorryLv
      `div` (if anxiety ≡ Relaxed then 3 else 1)
      -- Full nums are too big BUT we only care abt if they're divisible by the divBy values
      `mod` product (map (view (#test . #divBy)) apes) -- ergo worry lvls may be shrunk thus

parseApes ∷ String → [Ape]
parseApes = map parseApe . splitOn "\n\n" where
  parseApe = foldr parseLn (Ape 0 [] id 0 $ Test 0 0 0) . strLines
  parseLn ln = case strWords ln of
    ["Ape",id]               → set #id    $ read (take 1 id)
    ("Starting":_:xs)        → set #items $ map (read . filter (≢ ',')) xs
    [_,_,_,"old",sign,"old"] → set #op    $ \old → parseSign sign old old
    [_,_,_,"old",sign,n]     → set #op    $ parseSign sign (read n)
    ["Test:",_,_,n]          → set (#test . #divBy)   $ read n
    [_,"true:",_,_,_,id]     → set (#test . #onTrue)  $ read id
    [_,"false:",_,_,_,id]    → set (#test . #onFalse) $ read id
  parseSign = \case { "+" → (+); "*" → (*) }
