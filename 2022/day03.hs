import Data.Char (ord)
import Data.List (intersect)

sample :: String
sample = unlines [
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]

s2p :: String -> (String, String)
s2p s = let i = length s `div` 2 in (take i s, drop i s)

c2s :: Char -> Int
c2s c = if c < 'a'
  then 27 + ord c - ord 'A'
  else 1 + ord c - ord 'a'

mistake :: (String, String) -> Char
mistake = head . uncurry intersect

groups :: [String] -> [[String]]
groups [] = []
groups (a:b:c:ss) = [a,b,c] : groups ss

common :: [String] -> Char
common [a, b, c] = head $ intersect a $ intersect b c

main = do
  f <- readFile "day03.input.txt"
  putStr "Part 1: "
  print $ sum $ map (c2s . mistake . s2p) $ lines f
  putStr "Part 2: "
  print $ sum $ map (c2s.common) $ groups $ lines f