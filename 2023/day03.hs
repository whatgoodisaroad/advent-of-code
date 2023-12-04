import Control.Monad (guard)
import Data.Char (isDigit)

type Schematic = [String]
type Loc = (String, Int, Int, Int)

sample :: Schematic
sample = [
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  ]

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && not (isDigit c)

adj :: Schematic -> Int -> Int -> [(Int, Int)]
adj s r c = do
  r' <- [ 0 `max` pred r .. pred (length $ head s) `min` succ r ]
  c' <- [ 0 `max` pred c .. pred (length s) `min` succ c ]
  guard $ r /= r' || c /= c'
  return (r', c')

isSymbolAdjacent :: Schematic -> Int -> Int -> Bool
isSymbolAdjacent s r c = any (\(r, c) -> isSymbol $ s !! r !! c) $ adj s r c

fromRow :: String -> Int -> Int -> Maybe Loc -> [Loc]
fromRow [] _ _ Nothing = []
fromRow [] _ _ (Just b) = [b]
fromRow (cur:cs) r c Nothing = if isDigit cur
  then fromRow cs r (succ c) (Just ([cur], r, c, c))
  else fromRow cs r (succ c) Nothing
fromRow (cur:cs) r c (Just (buf, _, c1, c2)) = if isDigit cur
  then fromRow cs r (succ c) (Just (cur:buf, r, c1, c))
  else (buf, r, c1, c2) : fromRow cs r (succ c) Nothing

numberLocations :: Schematic -> [Loc]
numberLocations
  = concatMap (\(idx, row) -> fromRow row idx 0 Nothing)
  . zip [0..]

partNumbers :: Schematic -> [Int]
partNumbers s
  = map (\(s, _, _, _) -> read $ reverse s) $ filter isValid $ numberLocations s
  where
    isValid :: Loc -> Bool
    isValid (_, r, c1, c2) = any (isSymbolAdjacent s r) [c1 .. c2]

gearLocations :: Schematic -> [(Int, Int)]
gearLocations s = do
  r <- [0 .. pred $ length s]
  c <- [0 .. pred $ length $ head s]
  guard $ '*' == s !! r !! c
  return (r, c)

adjacentNumbers :: Schematic -> [(Int, Int)] -> [Int]
adjacentNumbers _ [] = []
adjacentNumbers s ((r, c):gears) = if length foundNumbers == 2
  then (head foundNumbers * foundNumbers !! 1) : next
  else next
  where
    foundNumbers :: [Int]
    foundNumbers = do
      (n, r1, c1, c2) <- numbers
      guard $ any (\(ar, ac) -> ar == r1 && c1 <= ac && c2 >= ac) (adj s r c)
      return $ read $ reverse n
    next = adjacentNumbers s gears
    numbers = numberLocations s

main = do
  f <- lines <$> readFile "day03.input.txt"
  putStr "Part 1: "
  print $ sum $ partNumbers f
  putStr "Part 2: "
  print $ sum $ adjacentNumbers f (gearLocations f)
