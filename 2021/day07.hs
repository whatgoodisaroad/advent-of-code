sample :: [Int]
sample = [16,1,2,0,4,2,7,1,2,14]

rng :: [Int] -> (Int, Int)
rng ps = (minimum ps, maximum ps)

simpleCost :: Int -> [Int] -> Int
simpleCost t = sum . map (\p -> abs $ p - t)

growingCost :: Int -> [Int] -> Int
growingCost t = sum . map (\p -> growthMemo !! (abs $ p - t))

growthMemo :: [Int]
growthMemo
  = flip map [0..]
  $ \n -> if n < 2 then n else (n + growthMemo !! (pred n))

costsByPosition :: (Int -> [Int] -> Int) -> [Int] -> [(Int, Int)]
costsByPosition f ps
  = map (\t -> (t, f t ps))
  $ uncurry enumFromTo
  $ rng ps

findMinCost :: [(Int, Int)] -> Int
findMinCost [(t, c)] = c
findMinCost ((t1, c1):(ps@((t2, c2):_)))
  = if c1 < c2
    then c1
    else findMinCost ps

main = do
  f <- readFile "day07.input.txt"
  let positions = read $ "[" ++ f ++ "]"
  putStr "Part 1: "
  print $ findMinCost $ costsByPosition simpleCost positions
  putStr "Part 2: "
  print $ findMinCost $ costsByPosition growingCost positions
