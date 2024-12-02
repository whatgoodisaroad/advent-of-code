sample :: [String]
sample = [
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9"
  ]

parse :: [String] -> [[Int]]
parse = map $ map read . words

safe :: [Int] -> Bool
safe (a:b:c) = s (a < b) $ a:b:c
  where
    s :: Bool -> [Int] -> Bool
    s asc (a:b:c)
      = ((asc && a < b) || (not asc && a > b))
      && (abs $ a - b) > 0 && (abs $ a - b) < 4
      && (s asc $ b:c)
    s _ _ = True
safe _ = True

damp :: [Int] -> Bool
damp l = or $ do
  i <- [ 0 .. length l ]
  return $ safe $ take i l ++ drop (succ i) l

main = do
  f <- lines <$> readFile "day02.input.txt"
  let reports = parse f
  putStr "Part 1: "
  print $ length $ filter safe reports
  putStr "Part 2: "
  print $ length $ filter damp reports
  