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

s = [
    "......",
    "..x...",
    "..x...",
    "......",
    "......",
    "......",
    "......",
    "......"
  ]

freqs :: [String] -> [Char]
freqs = nub . filter (/= '.') . concat

locations :: [String] -> Char -> [((Int, Int))]
locations m freq = do
  row <- [ 0 .. pred $ length m ]
  col <- [ 0 .. pred $ length $ m !! row ]
  guard $ m !! row !! col == freq
  return (row, col)

antinodes :: [((Int, Int))] -> [((Int, Int))]
antinodes ls = nub $ do
  i1 <- [0 .. pred $ length ls]
  let (r1, c1) = ls !! i1
  (r2, c2) <- drop i1 ls
  let (dr, dc) = (r1 - r2, c1 - c2)
  filter (not . flip elem ls) [(r1 + dr, c1 + dc), (r2 - dr, c2 - dc)]

bound :: [String] -> [(Int, Int)] -> [(Int, Int)]
bound m = let (rmax, cmax) = (length m, length $ m !! 0)
  in filter $ \(row, col)
    -> row >= 0
    && col >= 0
    && row < rmax
    && col < cmax

part1 :: [String] -> Int
part1 m
  = length
  $ nub
  $ bound m
  $ concat
  $ map (antinodes . locations m)
  $ freqs m

main = do
  f <- lines <$> readFile "day08.input.txt"
  putStr "Part 1: "
  print $ part1 f