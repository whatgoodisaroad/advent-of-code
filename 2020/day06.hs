import Data.List (intersect, nub)

splitIntoGroups :: [String] -> [[String]]
splitIntoGroups = sig []
  where
    sig :: [String] -> [String] -> [[String]]
    sig [] [] = [];
    sig b [] = [b]
    sig b (l:ll) =
      if null l
      then b : sig [] ll
      else sig (l:b) ll

groupSum :: [String] -> Int
groupSum = length . nub . concat

unanimousQs ::[String] -> String
unanimousQs = foldr1 intersect

main = do
  f <- readFile "day06.input.txt"
  putStr "Part 1: "
  print
    $ sum
    $ map groupSum
    $ splitIntoGroups
    $ lines f
  putStr "Part 2: "
  print
    $ sum
    $ map (length . unanimousQs)
    $ splitIntoGroups
    $ lines f
