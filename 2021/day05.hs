import Data.List (intersperse)
import Data.Map.Strict as Map hiding (foldr)
import Text.ParserCombinators.Parsec hiding (Line)

sampleData = [
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  ]

type Coord = (Int, Int)
type Line = (Coord, Coord)

parseFile :: String -> Either ParseError [Line]
parseFile = parse (fileP `sepBy` char '\n') "fail"
  where
    fileP = do
      let num = fmap read $ many1 digit
      x1 <- num
      char ','
      y1 <- num
      string " -> "
      x2 <- num
      char ','
      y2 <- num
      return ((x1, y1), (x2, y2))

enum :: Int -> Int -> [Int]
enum a b = if a < b then [a..b] else reverse [b..a]

points :: Bool -> Line -> [Coord]
points ortho ((x1, y1), (x2, y2)) =
  let
    xr = enum x1 x2
    yr = enum y1 y2
  in
  if x1 == x2
  then [(x1, y) | y <- yr]
  else if y1 == y2
  then [(x, y1) | x <- xr]
  else if ortho
  then []
  else zip xr yr

bumpMap :: Bool -> [Line] -> Map (Int, Int) Int
bumpMap ortho = foldr f empty . concatMap (points ortho)
  where
    f c m = insertWith (+) c 1 m

countOverlaps :: Bool -> [Line] -> Int
countOverlaps ortho = size . Map.filter (>1) . bumpMap ortho

main = do
  f <- readFile "day05.input.txt"
  let (Right lines) = parseFile $ f
  putStr "Part 1: "
  print $ countOverlaps True lines
  putStr "Part 2: "
  print $ countOverlaps False lines
