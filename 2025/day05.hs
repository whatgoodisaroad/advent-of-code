import Data.List (sortOn)

sample :: [String]
sample = [
    "3-5",
    "10-14",
    "16-20",
    "12-18",
    "",
    "1",
    "5",
    "8",
    "11",
    "17",
    "32"
  ]

parseRange :: String -> (Int, Int)
parseRange = let 
    p "" "" = (0, 0)
    p ls "" = (read ls, 0)
    p ls ('-':rs) = (read $ reverse ls, read rs)
    p ls (l:rs) = p (l:ls) rs
  in p ""

parseFile :: [String] -> ([(Int, Int)], [Int])
parseFile = let
    p rs [] = (rs, [])
    p rs ("":ls) = (rs, map read ls)
    p rs (r:ls) = p (parseRange r : rs) ls
  in p []

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh rs n = any (\(l, r) -> n >= l && n <= r) rs

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges = let
    merge [] = []
    merge [r] = [r]
    merge ((l1, r1):(l2, r2):rs) =
      if l2 <= r1 || succ r1 == l2 || l1 == l2
      then merge $ (l1, max r1 r2) : rs
      else (l1, r1) : (merge $ (l2, r2) : rs)
  in merge . sortOn fst

rangeSize :: (Int, Int) -> Int
rangeSize (l, r) = succ r - l

totalFresh :: [(Int, Int)] -> Int
totalFresh = sum . map rangeSize . mergeRanges

main :: IO ()
main = do
  f <- lines <$> readFile "day05.input.txt"
  let (ranges, ingredients) = parseFile f
  putStr "Part 1: "
  print $ length $ filter (isFresh ranges) ingredients
  putStr "Part 2: "
  print $ totalFresh ranges