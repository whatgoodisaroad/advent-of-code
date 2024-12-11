import Control.Monad (guard)
import Data.List (nub)

sample :: [String]
sample = [
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............"
  ]

freqs :: [String] -> [Char]
freqs = nub . filter (/= '.') . concat

locations :: [String] -> Char -> [(Int, Int)]
locations m freq = do
  row <- [ 0 .. pred $ length m ]
  col <- [ 0 .. pred $ length $ m !! row ]
  guard $ m !! row !! col == freq
  return (row, col)

harmonics :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
harmonics (r1, c1) (r2, c2)
  = flip map [1..] $ \i -> (r1 + (r2 - r1) * i, c1 + (c2 - c1) * i)

antinodes :: [(Int, Int)] -> [(Int, Int)]
antinodes ls = nub $ do
  i1 <- [0 .. pred $ length ls]
  let p1 = ls !! i1
  p2 <- drop (succ i1) ls
  let notAntenna = filter (not . flip elem ls) 
  (take 1 $ notAntenna $ harmonics p1 p2) ++ (take 1 $ notAntenna $ harmonics p2 p1)

resonances :: [String] -> [(Int, Int)] -> [(Int, Int)]
resonances m ls = do
  i1 <- [0 .. pred $ length ls]
  let p1 = ls !! i1
  p2 <- drop (succ i1) ls
  (takeWhile (inBounds m) $ harmonics p1 p2) ++ (takeWhile (inBounds m) $ harmonics p2 p1)

inBounds :: [String] -> (Int, Int) -> Bool
inBounds m (row, col)
  = let (rmax, cmax) = (length m, length $ m !! 0)
    in row >= 0 && col >= 0 && row < rmax && col < cmax

part1 :: [String] -> Int
part1 m
  = length
  $ nub
  $ filter (inBounds m)
  $ concatMap (antinodes . locations m)
  $ freqs m

part2 :: [String] -> Int
part2 m
  = length
  $ nub
  $ concatMap (resonances m . locations m)
  $ freqs m

main = do
  f <- lines <$> readFile "day08.input.txt"
  putStr "Part 1: "
  print $ part1 f
  putStr "Part 2: "
  print $ part2 f
