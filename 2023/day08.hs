import Data.Map.Strict (Map, fromList, keys, (!))
import Data.List (elemIndex, findIndex, last, transpose)
import Data.Maybe (fromJust)

type CamelMap = Map String (String, String)

sample1 :: [String]
sample1 = [
    "RL",
    "",
    "AAA = (BBB, CCC)",
    "BBB = (DDD, EEE)",
    "CCC = (ZZZ, GGG)",
    "DDD = (DDD, DDD)",
    "EEE = (EEE, EEE)",
    "GGG = (GGG, GGG)",
    "ZZZ = (ZZZ, ZZZ)"
  ]

sample2 :: [String]
sample2 = [
    "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
  ]

sample3 :: [String]
sample3 = [
    "LR",
    "",
    "11A = (11B, XXX)",
    "11B = (XXX, 11Z)",
    "11Z = (11B, XXX)",
    "22A = (22B, XXX)",
    "22B = (22C, 22C)",
    "22C = (22Z, 22Z)",
    "22Z = (22B, 22B)",
    "XXX = (XXX, XXX)"
  ]

parse :: [String] -> (String, CamelMap)
parse (dirs:"":nodes) = (dirs, nodeMap)
  where
    nodeMap = fromList $ flip map nodes $ \node ->
      let
        [name, "=", left, right] = words node
      in
        (name, (init $ tail left, init right))

nextStep :: Char -> CamelMap -> String -> String
nextStep d m p = case (m ! p, d) of
  ((l, _), 'L') -> l
  ((_, r), 'R') -> r

nextSteps :: Char -> CamelMap -> [String] -> [String]
nextSteps d m = map $ nextStep d m

steps :: (String -> Bool) -> Int -> [String] -> String -> CamelMap -> Int
steps f s !ps ds m = if all f ps
  then s
  else steps f (succ s) (nextSteps (ds !! (s `mod` length ds)) m ps) ds m

ghosts :: CamelMap -> [String]
ghosts = filter ((== 'A') . last) . keys

findZeroes :: Integer -> (String -> Bool) -> String -> String -> CamelMap -> [Integer]
findZeroes _ _ _ [] _ = []
findZeroes i f p (d:ds) m = if f p then i : next else next
  where
    next = findZeroes (succ i) f (nextStep d m p) ds m

findPeriods :: String -> CamelMap -> [String] -> [Integer]
findPeriods ds m [] = []
findPeriods ds m (p:ps) = period : findPeriods ds m ps
  where
    period = head $ findZeroes 0 ((== 'Z') . last) p (cycle ds) m

main :: IO ()
main = do
  (dirs, m) <- parse . lines <$> readFile "day08.input.txt"
  putStr "Part 1: "
  print $ steps (== "ZZZ") 0 ["AAA"] dirs m
  putStr "Part 2: "
  let periods = findPeriods dirs m $ ghosts m
  print $ foldr1 lcm periods
