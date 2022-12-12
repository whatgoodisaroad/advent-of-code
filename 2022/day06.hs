import Data.List (nub)

samples1 :: [String]
samples1 = [
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]

samples2 :: [String]
samples2 = [
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]

sop0 :: Int -> String -> Bool
sop0 n = (== n) . length . nub . take n

sop :: Int -> String -> Int
sop n s = if sop0 n s then n else succ $ sop n $ tail s

main = do
  f <- readFile "day06.input.txt"
  putStr "Part 1: "
  print $ sop 4 f
  putStr "Part 2: "
  print $ sop 14 f

