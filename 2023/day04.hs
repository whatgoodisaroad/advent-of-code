import Data.Bifunctor (first)
import Data.List (elemIndex, intersect)

sample :: [String]
sample = [
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  ]

data Card = Card [Int] [Int] deriving Show

parseCard :: String -> Card
parseCard line
  = Card (map read $ take i ns) (map read $ drop (succ i) ns)
  where
    (_:n:ns) = words line
    (Just i) = elemIndex "|" ns

matching :: Card -> Int
matching (Card w h) = length $ w `intersect` h

points :: Card -> Int
points c = case matching c of 0 -> 0; n -> 2 ^ pred n

copyingTotal :: [(Int, Card)] -> Int
copyingTotal [] = 0
copyingTotal ((qty, card):cards)
  = qty
  + copyingTotal (map (first (+qty)) (take m cards) ++ drop m cards)
  where
    m = matching card

main = do
  f <- lines <$> readFile "day04.input.txt"
  putStr "Part 1: "
  print $ sum $ map (points . parseCard) f
  putStr "Part 2: "
  print $ copyingTotal $ map ((1,) . parseCard) f
