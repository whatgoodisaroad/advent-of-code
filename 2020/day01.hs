import Data.List (sort)

toInts :: String -> String -> [Int]
toInts "" "" = []
toInts b "" = [read $ reverse b]
toInts b ('\n':ss) = (read $ reverse b) : toInts "" ss
toInts b (s:ss) = toInts (s:b) ss

numbers :: IO [Int]
numbers = fmap (toInts "") $ readFile "day01.input.txt"

findMatch :: Int -> Int -> [Int] -> Maybe Int
findMatch _ _ [] = Nothing
findMatch targetSum t (n:ns) =
  if t + n == targetSum
  then Just n
  else if t + n < targetSum
  then findMatch targetSum t ns
  else Nothing

findPair :: Int -> [Int] -> Maybe (Int, Int)
findPair _ [] = Nothing
findPair targetSum ns = case findMatch targetSum (last ns) (init ns) of
  Just n -> Just (last ns, n)
  Nothing -> findPair targetSum $ init ns

findTriplet :: [Int] -> Maybe (Int, Int, Int)
findTriplet [] = Nothing
findTriplet ns = let n0 = last ns in case findPair (2020 - n0) (init ns) of
  Just (n1, n2) -> Just (n0, n1, n2)
  Nothing -> findTriplet $ init ns

main = do
  nums <- fmap sort numbers
  putStr "Part 1: "
  print $ fmap (uncurry (*)) $ findPair 2020 nums
  putStr "Part 2: "
  print $ fmap (\(a,b,c)->a*b*c) $ findTriplet nums