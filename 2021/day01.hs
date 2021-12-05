exampleDepths = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

windows :: Int -> [Int] -> [Int]
windows size ns@(_:ns') =
  if length ns < size
  then []
  else (sum $ take size ns) : (windows size ns')

countIncreases :: [Int] -> Int
countIncreases ns = length $ filter id $ zipWith (<) ns $ tail ns

main = do
  f <- readFile "day01.input.txt"
  let depths = map (read :: String -> Int) $ lines f
  putStr "Part 1: "
  print $ countIncreases depths
  putStr "Part 2: "
  print $ countIncreases $ windows 3 depths