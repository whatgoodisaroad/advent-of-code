import Data.List (sort)

sample :: [String]
sample = [
    "3   4",
    "4   3",
    "2   5",
    "1   3",
    "3   9",
    "3   3"
  ]

parse :: [String] -> ([Int], [Int])
parse = unzip . p
  where
    p [] = []
    p (s:ss) = let [l, r] = words s in (read l, read r) : p ss

part1 :: [Int] -> [Int] -> Int
part1 l r = sum $ zipWith (\a b -> abs $ a - b) (sort l) (sort r)

part2 :: [Int] -> [Int] -> Int
part2 [] _ = 0
part2 (l:ls) r = l * (length $ filter (==l) r) + part2 ls r

main = do
  f <- lines <$> readFile "day01.input.txt"
  let (l, r) = parse f
  putStr "Part 1: "
  print $ part1 l r 
  putStr "Part 2: "
  print $ part2 l r
  