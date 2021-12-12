import Data.List (find)

sampleData = [
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
    "",
    "22 13 17 11  0",
    " 8  2 23  4 24",
    "21  9 14 16  7",
    " 6 10  3 18  5",
    " 1 12 20 15 19",
    "",
    " 3 15  0  2 22",
    " 9 18 13 17  5",
    "19  8  7 25 23",
    "20 11 10 24  4",
    "14 21 16 12  6",
    "",
    "14 21 17 24  4",
    "10 16 15  9 19",
    "18  8 23 26 20",
    "22 11 13  6  5",
    " 2  0 12  3  7"
  ]

type Game = ([Int], [Board])
type Board = [[Int]]

delineated :: Eq a => a -> [a] -> [a] -> [[a]]
delineated _ [] [] = []
delineated _ acc [] = [reverse acc]
delineated sep acc (c:cs) =
  if c /= sep
  then delineated sep (c:acc) cs
  else if null acc
  then delineated sep acc cs
  else (reverse acc):(delineated sep [] cs)

parseBoard :: [String] -> Board
parseBoard = map $ map read . delineated ' ' []

parseGame :: [String] -> Game
parseGame input = (sequence, boards)
  where
    sequence = map read $ delineated ',' [] $ head input
    boards = map parseBoard $ delineated "" [] $ tail input

transpose :: Board -> Board
transpose ns = do
  let range = [0 .. pred $ length ns]
  row <- range
  return $ do
    col <- range
    return $ ns !! col !! row

isWin :: [Int] -> Board -> Bool
isWin ns board
  =   (any (all $ flip elem ns) board)
  ||  (any (all $ flip elem ns) $ transpose board)

score :: [Int] -> Board -> Int
score ns@(n:_) board
  = (n*)
  $ sum
  $ filter (not . flip elem ns)
  $ concat board

firstWin :: [Int] -> Game -> Int
firstWin ns ((n:ns'), boards) = case find (isWin ns) boards of
  Nothing -> firstWin (n:ns) (ns', boards)
  Just board -> score ns board

lastWin :: [Int] -> [Board] -> Int
lastWin (n:ns) boards = case find (not . isWin ns) boards of
  Nothing -> lastWin (ns) boards
  Just board -> score (n:ns) board

main = do
  f <- readFile "day04.input.txt"
  let game@(ns, boards) = parseGame $ lines f
  putStr "Part 1: "
  print $ firstWin [] game
  putStr "Part 2: "
  print $ lastWin (reverse ns) $ filter (isWin ns) boards
