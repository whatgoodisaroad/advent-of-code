import Data.List (sort)

toPair :: String -> ([Bool], [Bool])
toPair s = (
    map (=='B') $ take 7 s,
    map (=='R') $ drop 7 s
  )

asBinary :: [Bool] -> Int
asBinary = asBinary' . reverse
  where
    asBinary' [] = 0
    asBinary' (b:bs) = (if b then 1 else 0) + 2 * asBinary' bs

toSeat :: ([Bool], [Bool]) -> (Int, Int, Int)
toSeat (rb, cb) = (r, c, r * 8 + c)
  where
    r = asBinary rb
    c = asBinary cb

findGap :: [Int] -> Maybe Int
findGap (a:b:c) =
  if b - a > 1
  then Just $ succ a
  else findGap (b:c)
findGap _ = Nothing

main = do
  f <- readFile "day05.input.txt"
  let ids = sort $ map (\(_, _, i) -> i) $ map toSeat $ map toPair $ lines f
  putStr "Part 1: "
  print $ maximum ids
  putStr "Part 2: "
  print $ findGap ids