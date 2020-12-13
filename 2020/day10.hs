import Data.List (sort)
import Data.Map.Strict as Map (Map, empty, insert, lookup, (!))

getDeviceJoltage :: [Int] -> Int
getDeviceJoltage = (3+) . maximum

differences :: [Int] -> (Int, Int, Int)
differences [] = (0, 0, 0)
differences [n] = (0, 0, 0)
differences (a:b:c) = case (b - a, differences (b:c)) of
  (1, (d, e, f)) -> (succ d, e, f)
  (2, (d, e, f)) -> (d, succ e, f)
  (3, (d, e, f)) -> (d, e, succ f)

prod :: (Int, Int, Int) -> Int
prod (a, _, b) = a * b

validSteps :: [Int] -> Int -> [Int]
validSteps as a = flip filter as $ \b -> b > a && b <= a + 3

getOrFill :: [Int] -> Map Int Int -> Int -> Int -> (Int, Map Int Int)
getOrFill as m key target = case Map.lookup key m of
  Just l -> (l, m)
  Nothing -> let m' = fillChain m as key target in (m' ! key, m')

fillChain :: Map Int Int -> [Int] -> Int -> Int -> Map Int Int
fillChain m as key target =
  if key == target
  then insert key 1 m
  else let (cq, m') = foldr g (0, m) (validSteps as key) in insert key cq m'
    where
      g :: Int -> (Int, Map Int Int) -> (Int, Map Int Int)
      g a (acc, m') = (acc + cq, m'')
        where
          (cq, m'') = getOrFill as m' a target

example1, example2 :: [Int]
example1 = [
    16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4
  ]
example2 = [
    28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1,
    32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3
  ]

main = do
  f <- readFile "day10.input.txt"
  let adapters = map (read :: String -> Int) $ lines f
  let dj = getDeviceJoltage adapters
  putStr "Part 1: "
  print $ prod $ differences $ [0] ++ sort adapters ++ [dj]
  putStr "Pars 2: "
  print $ fst $ getOrFill (sort $ dj:adapters) empty 0 dj
