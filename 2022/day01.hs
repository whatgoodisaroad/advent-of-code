import Data.List (last, sort)

sample :: String
sample = unlines [
    "1000",
    "2000",
    "3000",
    "",
    "4000",
    "",
    "5000",
    "6000",
    "",
    "7000",
    "8000",
    "9000",
    "",
    "10000"
  ]

gs :: [String] -> [String] -> [[String]]
gs acc [] = [acc]
gs acc ("":ss) = acc : gs [] ss
gs acc (s:ss) = gs (s:acc) ss

f2s :: String -> [Int]
f2s = map (sum . map read) . gs [] . lines

main = do
  f <- readFile "day01.input.txt"
  let sums = sort $ f2s f
  putStr "Part 1: "
  print $ last sums
  putStr "Part 2: "
  print $ sum $ take 3 $ reverse $ sums