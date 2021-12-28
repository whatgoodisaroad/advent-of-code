sampleSeed :: [Int]
sampleSeed = [3,4,3,1,2]

num :: Int -> Int -> [Int]
num 9 _ = []
num m n = (if m == n then 1 else 0) : num (succ m) n

addS :: [Int] -> [Int] -> [Int]
addS = zipWith (+)

mulS :: Int -> [Int] -> [Int]
mulS n = map (n*)

nextS :: [Int] -> [Int]
nextS (z:s)
  = addS (s ++ [0])
  $ addS (mulS z $ num 0 6)
  $ mulS z
  $ num 0 8

seedToState :: [Int] -> [Int]
seedToState = foldr1 addS . map (num 0)

days :: [Int] -> [[Int]]
days seed = seed : (days $ nextS seed)

main = do
  f <- readFile "day06.input.txt"
  let seed = seedToState $ read $ "[" ++ f ++ "]"
  putStr "Part 1: "
  print $ sum $ days seed !! 80
  putStr "Part 2: "
  print $ sum $ days seed !! 256