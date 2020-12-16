import Data.List (maximumBy, minimumBy)

fileToSchedule :: String -> (Int, [Int])
fileToSchedule file = (read $ head ls, bs "" (ls !! 1))
  where
    ls = lines file
    bs :: String -> String -> [Int]
    bs acc "" = [read $ reverse acc]
    bs acc (c:cs) = case c of
      ',' -> if null acc then bs acc cs else (read $ reverse acc) : (bs "" cs)
      'x' -> bs acc cs
      _ -> bs (c:acc) cs

nextBus :: (Int, [Int]) -> Int
nextBus (t, bs) = fst $ minimumBy g $ map (\b -> (b, b - (t `mod` b))) bs
  where
    (_, a) `g` (_, b) = a `compare` b

nextBusTimeProduct :: (Int, [Int]) -> Int
nextBusTimeProduct (t, bs) = nb * t'
  where
    nb = nextBus (t, bs)
    t' = nb - (t `mod` nb)

example = "939\n7,13,x,x,59,x,31,19"

main = do
  f <- readFile "day13.input.txt"
  putStr "Day 1: "
  print $ nextBusTimeProduct $ fileToSchedule example -- f
