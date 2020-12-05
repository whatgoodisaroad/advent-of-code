massToFuel :: Double -> Int
massToFuel = (max 0) . (\n -> n-2) . (floor :: Double -> Int) . (/3.0)

massToFuel2 :: Double -> Int
massToFuel2 m =
  if f == 0
  then 0
  else f + massToFuel2 fi
  where
    f = massToFuel m
    fi = fromIntegral f

main = do
  f <- readFile "day01.input.txt"
  let masses = map (read :: String -> Double) $ lines f
  putStr "Part 1: "
  print $ sum $ map massToFuel masses
  putStr "Part 2: "
  print $ sum $ map massToFuel2 masses