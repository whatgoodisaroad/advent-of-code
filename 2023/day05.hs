import Data.List (findIndex)
import Data.Maybe (fromJust)
import Prelude hiding (traverse)

type Triple = (Int, Int, Int)
type Range = (Int, Int)

sample :: [String]
sample = [
    "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4"
  ]

groups :: [String] -> [String] -> [[String]]
groups [] b = [b]
groups ([]:ls) b = b : groups ls []
groups (l:ls) b = groups ls $ b ++ [l]

parseMap :: [String] -> [Triple]
parseMap = map ((\[d, s, r] -> (d, s, r)) . map read . words) . tail

parseFile :: [String] -> ([Int], [[Triple]])
parseFile ls = let ([seedLine]:gs) = groups ls [] in (
    map read $ tail $ words seedLine,
    map parseMap gs
  )

isIn :: Int -> Range -> Bool
x `isIn` (s, l) = x >= s && x <= s + l

eval :: Int -> [Triple] -> Int
eval i [] = i
eval i ((d, s, r):ts) = if i `isIn` (s, r) then d + (i - s) else eval i ts

traverse :: [[Triple]] -> Int -> Int
traverse = flip $ foldl eval

seedRanges :: [Int] -> [Range]
seedRanges [] = []
seedRanges (s:r:ss) = (s, r) : seedRanges ss

sources :: [Triple] -> Int -> [Int]
sources m i = (if i `eval` m == i then (i:) else id) $ sources' m
  where
    sources' :: [Triple] -> [Int]
    sources' [] = []
    sources' ((d, s, r):ms)
      = (if i `isIn` (d, r) then ((s + (i - d)):) else id)
      $ sources' ms

traversalSources :: [[Triple]] -> Int -> [Int]
traversalSources [] i = [i]
traversalSources (m:ms) i = do
  i' <- sources m i
  traversalSources ms i'

isAnySeed :: [Range] -> Int -> Bool
isAnySeed [] _ = False
isAnySeed (r:rs) i = i `isIn` r || isAnySeed rs i

anyIsAnySeed :: [Range] -> [Int] -> Bool
anyIsAnySeed rs = any (isAnySeed rs)

main :: IO ()
main = do
  f <- lines <$> readFile "day05.input.txt"
  -- let f = sample
  let (seeds, maps) = parseFile f
  putStr "Part 1: "
  print $ minimum $ map (traverse maps) seeds
  putStr "Part 2: "
  print 
    $ fromJust
    $ findIndex 
      (anyIsAnySeed (seedRanges seeds) . traversalSources (reverse maps))
      [0..]
