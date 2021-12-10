sampleData = [
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "01010",
    "00010"
  ]

bin2Counters :: String -> [Int]
bin2Counters = map $ \c -> if c == '0' then -1 else 1

counters2Bin :: [Int] -> String
counters2Bin = map $ \n -> if n < 0 then '0' else '1'

negateBin :: String -> String
negateBin = map $ \c -> if c == '0' then '1' else '0'

bin2Int :: String -> Int
bin2Int = bin2Int' . reverse
  where
    bin2Int' [] = 0
    bin2Int' (n:ns) = 2 * bin2Int' ns + if n == '0' then 0 else 1

sumColumns :: [Int] -> [Int] -> [Int]
sumColumns [] _ = []
sumColumns (a:as) (b:bs) = (a + b) : sumColumns as bs

findGamma :: [String] -> String
findGamma
  = counters2Bin
  . foldr (\a b -> bin2Counters a `sumColumns` b) (repeat 0)

firstBitCommonality :: Bool -> [String] -> Char
firstBitCommonality most ns
  = case (sum (map (head.bin2Counters) ns) < 0, most) of
    (True, True) -> '0'
    (False, True) -> '1'
    (True, False) -> '1'
    (False, False) -> '0'

findRating :: Bool -> [String] -> [String]
findRating o2 [n] = [n]
findRating o2 ns
  = map (g:)
  $ findRating o2
  $ map tail
  $ filter (\n -> head n == g) ns
  where
    g = firstBitCommonality o2 ns

main = do
  f <- readFile "day03.input.txt"
  let dat = filter (not . null) $ lines f
  putStr "Part 1: "
  let gammaBin = findGamma dat
  let gamma = bin2Int gammaBin
  let epsilonBin = negateBin gammaBin
  let epsilon = bin2Int $ epsilonBin
  print $ gamma * epsilon
  putStr "Part 2: "
  let [o2] = map bin2Int $ findRating True dat
  let [co2] = map bin2Int $ findRating False dat
  print $ o2 * co2