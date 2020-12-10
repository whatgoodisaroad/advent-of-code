import Data.List (sort)

isSumOfPair :: [Int] -> Int -> Maybe (Int, Int)
isSumOfPair [] _ = Nothing
isSumOfPair pre n = case isSumWith (init pre) (last pre) n of
  r@(Just _) -> r
  _ -> isSumOfPair (init pre) n

isSumWith :: [Int] -> Int -> Int -> Maybe (Int, Int)
isSumWith [] _ _ = Nothing
isSumWith (p1:ps) p2 n =
  if p1 + p2 == n
  then Just (p1, p2)
  else if p1 + p2 > n
  then Nothing
  else isSumWith ps p2 n

firstNonPair :: [Int] -> [Int] -> Maybe Int
firstNonPair [] _ = Nothing
firstNonPair _ [] = Nothing
firstNonPair ps (n:ns) = case isSumOfPair (sort ps) n of
  Nothing -> Just n
  _ -> firstNonPair ((tail ps) ++ [n]) ns

findContiguousSum :: [Int] -> Int -> Maybe [Int]
findContiguousSum [] _ = Nothing
findContiguousSum ps t = case fcs [] ps t of
  Just l -> Just l
  Nothing -> findContiguousSum (tail ps) t
  where
    fcs _ [] _ = Nothing
    fcs acc (p:ps) t =
      if sum acc + p == t
      then Just $ acc ++ [p]
      else if sum acc + p > t
      then Nothing
      else fcs (acc ++ [p]) ps t

minPlusMax :: [Int] -> Int
minPlusMax ns = minimum ns + maximum ns

example = [
    35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576
  ]

preambleSize = 25

main = do
  ns <- fmap (map read . lines) $ readFile "day09.input.txt"
  putStr "Part 1: "
  let fnp = (\(Just n) -> n)
          $ firstNonPair (take preambleSize ns) (drop preambleSize ns)
  print $ fnp
  putStr "Part 2: "
  print $ fmap minPlusMax $ findContiguousSum ns fnp