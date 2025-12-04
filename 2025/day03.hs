import Data.Char (digitToInt)
import Data.List (findIndex)

sample :: [String]
sample = [
    "987654321111111",
    "811111111111119",
    "234234234234278",
    "818181911112111"
  ]

j :: Int -> String -> Int
j 0 _ = 0
j _ [] = 0
j 1 s = maximum $ map digitToInt s
j n s = let
    x = length s - (pred n)
    y = take x s
    d = maximum $ map digitToInt y
    Just i = findIndex (==d) $ map digitToInt y
  in
    d .: j (pred n) (drop (succ i) s)

(.:) :: Int -> Int -> Int
c .: n = read $ show c ++ show n

main :: IO ()
main = do
  f <- lines <$> readFile "day03.input.txt"
  putStr "Part 1: "
  print $ sum $ map (j 2) f
  putStr "Part 2: "
  print $ sum $ map (j 12) f
