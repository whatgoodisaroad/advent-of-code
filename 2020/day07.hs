import Data.List (nub)
import Data.Map.Strict as Map (Map, empty, insert, lookup, toList, (!))
import Text.ParserCombinators.Parsec

type Rule = (String, [(String, Int)])

wordP :: GenParser Char st String
wordP = many $ oneOf ['a'..'z']

bagP :: GenParser Char st String
bagP = do
  t1 <- wordP
  space
  t2 <- wordP
  string " bag"
  optional $ char 's'
  return $ t1 ++ " " ++ t2

numberP :: GenParser Char st Int
numberP = (many $ oneOf ['0'..'9']) >>= (return.read)

noneP :: GenParser Char st [(String, Int)]
noneP = string "no other bags" >> return []

constraintP :: GenParser Char st (String, Int)
constraintP = do
  q <- numberP
  space
  bag <- bagP
  return (bag, q)

constraintsP :: GenParser Char st [(String, Int)]
constraintsP = try noneP <|> (constraintP `sepBy` (string ", "))

ruleP :: GenParser Char st Rule
ruleP = do
  bag <- bagP
  string " contain "
  cs <- constraintsP
  char '.'
  eof
  return (bag, cs)

hasConstraint :: String -> Rule -> Bool
hasConstraint t (_, cs) = t `elem` map fst cs

findContainers :: [Rule] -> [String] -> [String]
findContainers rules bags =
  if next == bags
  then bags
  else findContainers rules next
  where
    next
      = nub
      $ (++) bags
      $ map fst
      $ concatMap (\c -> filter (hasConstraint c) rules) bags

type RuleMap = Map String ([(String, Int)], Maybe Int)

toMap :: [Rule] -> RuleMap
toMap = flip foldr empty $ \(n, cs) -> insert n (cs, Nothing)

fillByName :: String -> RuleMap -> (RuleMap, Int)
fillByName name m = case m ! name of
  (_, Just n) -> (m, n)
  ([], _) -> (insert name ([], Just 1) m, 1)
  (cs, _) -> (insert name (cs, Just $ succ q) m', succ q)
    where
      (m', q) = foldr g (m, 0) cs
      g (cn, amt) (m', acc) = (m'', req * amt + acc)
        where
          (m'', req) = fillByName cn m'

example1 = [
    "light red bags contain 1 bright white bag, 2 muted yellow bags.",
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
    "bright white bags contain 1 shiny gold bag.",
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
    "faded blue bags contain no other bags.",
    "dotted black bags contain no other bags."
  ]

example2 = [
    "shiny gold bags contain 2 dark red bags.",
    "dark red bags contain 2 dark orange bags.",
    "dark orange bags contain 2 dark yellow bags.",
    "dark yellow bags contain 2 dark green bags.",
    "dark green bags contain 2 dark blue bags.",
    "dark blue bags contain 2 dark violet bags.",
    "dark violet bags contain no other bags."
  ]

main = do
  db <- readFile "day07.input.txt"
  case sequence $ map (parse ruleP "failed") $ lines db of
    Left err -> putStrLn $ show err
    Right rules -> do
      putStr "Part 1: "
      print
        $ length
        $ findContainers rules
        $ map fst
        $ filter (hasConstraint "shiny gold") rules
      putStr "Part 2: "
      print $ pred $ snd $ fillByName "shiny gold" $ toMap rules
