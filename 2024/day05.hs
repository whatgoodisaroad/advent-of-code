import Data.Function (fix)
import Data.List (findIndex, findIndices, sortOn)
import Data.Maybe (catMaybes)

sample :: [String]
sample = [
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13",
    "",
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,29,13",
    "75,97,47,61,53",
    "61,13,29",
    "97,13,75,29,47"
  ]

type Rule = (Int, Int)
type Update = [Int]

parseRule :: String -> Rule
parseRule s = (read $ take 2 s, read $ drop 3 s)

parseUpdate :: String -> Update
parseUpdate [p1, p2] = return $ read [p1, p2]
parseUpdate (p1:p2:',':s) = read [p1, p2] : parseUpdate s

parse :: [String] -> ([Rule], [Update])
parse f = let
    (rules, (_:updates)) = break null f
  in
    (map parseRule rules, map parseUpdate $ updates)

violation :: Rule -> Update -> Maybe (Int, Int)
violation (r1, r2) u = do
  i1 <- findIndex (==r2) u
  i2 <- findIndex (==r1) $ drop i1 u
  return (i1, i1 + i2)

validUpdate :: [Rule] -> Update -> Bool
validUpdate rs u = all (flip validRule u) rs
  where
    validRule r u = violation r u == Nothing

middle :: Update -> Int
middle u = u !! (length u `div` 2)

part1 :: [Rule] -> [Update] -> Int
part1 rs us
  = sum
  $ map middle
  $ filter (validUpdate rs) us

swap :: Update -> Int -> Int -> Update
swap u i1 i2
  =  take i1 u
  ++ [u !! i2]
  ++ (take (i2 - i1 - 1) $ drop (succ i1) u)
  ++ [u !! i1]
  ++ drop (succ i2) u 

repairUpdate :: [Rule] -> Update -> Update
repairUpdate rs u
  = foldr (\v u -> uncurry (swap u) v) u
  $ sortOn snd
  $ catMaybes $ map (flip violation u) rs
 
main = do
  f <- lines <$> readFile "day05.input.txt"
  let (rules, updates) = parse f
  putStr "Part 1: "
  print $ part1 rules updates
