import Control.Monad (guard)
import Data.List (nub, sort)

sample = [
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  ]

parseDepths :: String -> [[Int]]
parseDepths = map (map (read . return)) . lines

maxes :: [[Int]] -> (Int, Int)
maxes i = (pred $ length i, pred $ length $ i !! 0)

ortho :: [[Int]] -> Int -> Int -> [(Int, Int)]
ortho i row col = filter f [
      (row, pred col),
      (row, succ col),
      (pred row, col),
      (succ row, col)
    ]
  where
    (rowMax, colMax) = maxes i
    f (r, c) = r >= 0 && c >= 0 && r <= rowMax && c <= colMax

minima :: [[Int]] -> [(Int, Int, Int)]
minima i = do
  let (rowMax, colMax) = maxes i
  row <- [0 .. rowMax]
  col <- [0 .. colMax]
  let cell = i !! row !! col
  guard
    $ all (\(row', col') ->  cell < i !! row' !! col')
    $ ortho i row col
  return (cell, row, col)

riskLevel :: [(Int, Int, Int)] -> Int
riskLevel = sum . map (\(d, _, _) -> succ d)

basinSize :: [[Int]] -> [(Int, Int)] -> [(Int, Int)] -> Int
basinSize _ [] visited = length visited
basinSize i f v = basinSize i f' $ v ++ f
  where
    f' = nub $ do
      (row, col) <- f
      (row', col') <- ortho i row col
      guard $ not $ elem (row', col') v
      guard $ 9 /= i !! row' !! col'
      return (row', col')

minToBasinSize :: [[Int]] -> (Int, Int, Int) -> Int
minToBasinSize i (_, row, col) = basinSize i [(row, col)] []

main = do
  f <- readFile "day09.input.txt"
  let depths = parseDepths f
  putStr "Part 1: "
  print $ riskLevel $ minima depths
  putStr "Part 2: "
  print
    $ product
    $ take 3
    $ reverse
    $ sort
    $ map (minToBasinSize depths)
    $ minima depths
