import Data.List (intersect)

sample :: String
sample = unlines [
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8"
  ]

indexOf :: Eq a => a -> [a] -> Int
indexOf a' (a:as) = if a' == a then 0 else 1 + indexOf a' as

split :: Eq a => a -> [a] -> ([a], [a])
split c s = let i = indexOf c s in (take i s, drop (succ i) s)

type Assignment = (Int, Int)

l2as :: String -> (Assignment, Assignment)
l2as s = let
    (p1, p2) = split ',' s
    (p1a, p1b) = split '-' p1
    (p2a, p2b) = split '-' p2
  in
    ((read p1a, read p1b), (read p2a, read p2b))

contains,
  eitherContains,
  overlaps :: Assignment -> Assignment -> Bool
contains (p1a, p1b) (p2a, p2b) = p1a <= p2a && p2b <= p1b
eitherContains a1 a2 = a1 `contains` a2 || a2 `contains` a1
overlaps p1 p2
  = not
  $ null
  $ intersect (uncurry enumFromTo p1) (uncurry enumFromTo p2)

main = do
  f <- readFile "day04.input.txt"
  putStr "Part 1: "
  print
    $ length
    $ filter id
    $ map (uncurry eitherContains . l2as)
    $ filter (not . null)
    $ lines f
  putStr "Part 2: "
  print
    $ length
    $ filter id
    $ map (uncurry overlaps . l2as)
    $ filter (not . null)
    $ lines f
