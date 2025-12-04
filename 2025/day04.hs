import Control.Monad (guard)
import Data.Function (fix)

sample :: [String]
sample = [
    "..@@.@@@@.",
    "@@@.@.@.@@",
    "@@@@@.@.@@",
    "@.@@@@..@.",
    "@@.@@@@.@@",
    ".@@@@@@@.@",
    ".@.@.@.@@@",
    "@.@@@.@@@@",
    ".@@@@@@@@.",
    "@.@.@@@.@."
  ]

neighbors :: [String] -> Int -> Int -> Int
neighbors g r c = length $ do
  r' <- [pred r .. succ r]
  c' <- [pred c .. succ c]
  guard
    $ r' >= 0
    && r' < length g
    && c' >= 0
    && c' < (length $ head g)
    && (not $ r == r' && c == c')
    && '@' == g !! r' !! c'
  return True

mark :: [String] -> [String]
mark g = do
  r <- [0 .. (pred $ length g)]
  return $ do
    c <- [0 .. (pred $ length $ head g)]
    return $ if '@' == g !! r !! c && neighbors g r c < 4
      then 'x'
      else g !! r !! c

free :: [String] -> Int
free = length . filter (=='x') . concat . mark

removable :: [String] -> Int
removable = length . filter (=='x') . concat . final
  where
    final :: [String] -> [String]
    final g = let g' = mark g in if g == g' then g else final g'

main :: IO ()
main = do
  f <- lines <$> readFile "day04.input.txt"
  putStr "Part 1: "
  print $ free f
  putStr "Part 2: "
  print $ removable f