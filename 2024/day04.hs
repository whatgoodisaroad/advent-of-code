import Control.Monad (guard)

sample :: [String]
sample = [
    "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX"
  ]

dirs :: [(Int, Int)]
dirs = do
  dr <- [-1, 0, 1]
  dc <- [-1, 0, 1]
  guard $ dr /= 0 || dc /= 0
  return (dr, dc)

enumCoords :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
enumCoords _ _ 0 = []
enumCoords (r, c) (dr, dc) n
  = (r, c) : enumCoords (r + dr, c + dc) (dr, dc) (pred n)

stringByCoords :: [String] -> [(Int, Int)] -> String
stringByCoords _ [] = []
stringByCoords g ((r, c):cs)
  = if r < 0 || c < 0 || r >= length g || c >= (length $ g !! r)
    then []
    else g !! r !! c : stringByCoords g cs

findCoords :: [String] -> Char -> [(Int, Int)]
findCoords g x = do
  r <- [0 .. pred $ length g ]
  c <- [0 .. pred $ length $ g !! r ]
  guard $ g !! r !! c == x
  return (r, c)

part1 :: [String] -> String -> Int
part1 haystack needle@(c:_) = length $ filter (== needle) $ do 
  (r, c) <- findCoords haystack c
  (dr, dc) <- dirs
  return
    $ stringByCoords haystack
    $ enumCoords (r, c) (dr, dc)
    $ length needle

part2 :: [String] -> String -> Char -> Int
part2 haystack needle pivot = length $ do
  (r, c) <- findCoords haystack pivot
  let coords1 = [(pred r, pred c), (r, c), (succ r, succ c)]
  let coords2 = [(succ r, pred c), (r, c), (pred r, succ c)]
  let f1 = stringByCoords haystack coords1
  let f2 = stringByCoords haystack coords2
  guard
    $ (f1 == needle || f1 == reverse needle)
    && (f2 == needle || f2 == reverse needle)
  return True

main = do
  f <- lines <$> readFile "day04.input.txt"
  putStr "Part 1: "
  print $ part1 f "XMAS"
  putStr "Part 2: "
  print $ part2 f "MAS" 'A'