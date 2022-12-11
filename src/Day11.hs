module Day11 where

import Control.Lens    (ix, over, set, view)
import Data.List.Extra (drop1, dropEnd1, splitOn, takeEnd)
import Relude.Unsafe   (read, (!!))

main ∷ IO ()
main = do
  monkeys ← parseMonkeys <$> readFile "./inputs/Day11.txt"
  putStr $ strUnlines
    [ "Part 1:", show . monkeyBusinessLv $ runRounds 20 Relaxed monkeys
    , "Part 2:", show . monkeyBusinessLv $ runRounds 10000 Anxious monkeys
    ]

data Monkey =
  Monkey { id ∷ Int, items ∷ [Int], op ∷ Int → Int, inspected ∷ Int, test ∷ Test }
  deriving (Generic)

data Test = Test { divBy ∷ Int, onTrue ∷ Int, onFalse ∷ Int } deriving (Generic)

data WorryLv = Relaxed | Anxious deriving (Eq)

monkeyBusinessLv ∷ [Monkey] → Int
monkeyBusinessLv = product . takeEnd 2 . sort . map (view #inspected)

runRounds ∷ Int → WorryLv → [Monkey] → [Monkey]
runRounds howMany worry = iterate runRound ⋙ (!! howMany) where
  runRound apes         = foldl' runMonkey apes $ map (view #id) apes
  runMonkey apes n      = foldl' (runItem n) apes $ view #items (apes !! n)
  runItem senderN apes itemWorryLv = apes
    & over (ix senderN . #items)     drop1
    & over (ix senderN . #inspected) (+1)
    & over (ix recipientN . #items)  (++ [itemNewWorryLv])
    where
    Monkey { op, test } = apes !! senderN
    Test { divBy, onTrue, onFalse } = test
    recipientN = if itemNewWorryLv `mod` divBy ≡ 0 then onTrue else onFalse
    itemNewWorryLv = op itemWorryLv
      `div` (if worry ≡ Relaxed then 3 else 1)
      -- Full nums are too big BUT we only care abt if they're divisible by the divBy values
      `mod` product (apes <&> view (#test . #divBy)) -- ergo worry levels may be shrunk thus

parseMonkeys ∷ String → [Monkey]
parseMonkeys = map parseMonkey . splitOn "\n\n" where
  parseMonkey = foldr parseLn (Monkey 0 [] id 0 $ Test 0 0 0) . strLines
  parseLn ln = case strWords ln of
    ["Monkey",id]            → set #id    $ read (take 1 id)
    ("Starting":_:xs)        → set #items $ map (read . filter (≢ ',')) xs
    [_,_,_,"old",sign,"old"] → set #op    $ \old → parseSign sign old old
    [_,_,_,"old",sign,n]     → set #op    $ parseSign sign (read n)
    ["Test:",_,_,n]          → set (#test . #divBy)   $ read n
    [_,"true:",_,_,_,id]     → set (#test . #onTrue)  $ read id
    [_,"false:",_,_,_,id]    → set (#test . #onFalse) $ read id
  parseSign = \case { "+" → (+); "*" → (*) }
