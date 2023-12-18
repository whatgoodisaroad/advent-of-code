d1 :: [Int] -> [Int]
d1 ns = zipWith (-) (tail ns) ns

dn :: [Int] -> [[Int]]
dn ns = let ds = d1 ns in if all (== 0) ds then [ns, ds] else ns : dn ds

predict :: [Int] -> Int
predict = sum . map last . dn

parseFile :: String -> [[Int]]
parseFile = map (map read . words) . lines

main :: IO ()
main = do
  seqs <- parseFile <$> readFile "day09.input.txt"
  putStr "Part 1: "
  print $ sum $ map predict seqs
  putStr "Part 2: "
  print $ sum $ map (predict . reverse) seqs
