import Data.Either (fromRight)
import Data.List (sort)
import Data.Map.Strict (Map, (!), adjust, empty, insert, keys, elems, fromList)
import Text.ParserCombinators.Parsec

data Operand = Mul | Add deriving Show
data Operation = OpSelf Operand | OpN Operand Int deriving Show
data Monkey = M Int [Int] Operation Int Int Int Int deriving Show
type MonkeyState = Map Int Monkey

intP :: (Integral i, Read i) => GenParser Char st i
intP = fmap read $ many1 digit

operationP :: GenParser Char st Operation
operationP = do
  string "new = old "
  operator <- oneOf "+*"
  char ' '
  operand <- (fmap Left $ string "old") <|> fmap Right intP
  return $ case (operator, operand) of
    ('*', Left "old") -> OpSelf Mul
    ('+', Left "old") -> OpSelf Add
    ('*', Right n) -> OpN Mul n
    ('+', Right n) -> OpN Add n

monkeyP :: GenParser Char st Monkey
monkeyP = do
  -- Header
  string "Monkey "
  num <- intP
  string ":\n"
  -- Items
  string "  Starting items: "
  items <- intP `sepBy` (try $ string ", ")
  char '\n'
  -- Operation
  string "  Operation: "
  operation <- operationP
  char '\n'
  -- Test
  string "  Test: divisible by "
  threshold <- intP
  char '\n'
  string "    If true: throw to monkey "
  t <- intP
  char '\n'
  string "    If false: throw to monkey "
  f <- intP
  char '\n'
  return $ M num items operation threshold t f 0

getModulus :: MonkeyState -> Int
getModulus
  = foldr1 lcm
  . map (\(M _ _ _ t _ _ _) -> t)
  . elems

parseMonkeys :: String -> MonkeyState
parseMonkeys
  = fromList
  . map (\m@(M num _ _ _ _ _ _) -> (num, m))
  . fromRight []
  . parse (monkeyP `sepBy` char '\n') "failed"

apply :: Operation -> Int -> Int
apply (OpSelf Mul) m = m * m
apply (OpSelf Add) m = m + m
apply (OpN Mul n) m = m * n
apply (OpN Add n) m = m + n 

appendItem :: Int -> Int -> MonkeyState -> MonkeyState
appendItem target item
  = flip adjust target 
  $ \(M num is op threshold t f c) -> M num (is ++ [item]) op threshold t f c

insp1 :: Int -> Bool -> Monkey -> (Monkey, Int, Int)
insp1 modulus disc (M num (i:is) op threshold t f c) = let
    i' = apply op i
    i'' = if disc then i' `div` 3 else i'
    i''' = i'' `mod` modulus
    target = if i''' `mod` threshold == 0 then t else f
  in
    (M num is op threshold t f $ succ c, target, i''')

inspM :: Int -> Bool -> Int -> MonkeyState -> MonkeyState
inspM modulus disc n ms = case (ms ! n) of
  (M _ [] _ _ _ _ _) -> ms
  m -> let 
      (m', t, i) = insp1 modulus disc m
    in inspM modulus disc n $ appendItem t i $ insert n m' ms

monkeyRound :: Int -> Bool -> MonkeyState -> MonkeyState
monkeyRound modulus disc ms = r (sort $ keys ms) ms
  where
    r :: [Int] -> MonkeyState -> MonkeyState
    r [] ms = ms
    r (n:ns) ms = r ns $ inspM modulus disc n ms

getRound :: Int -> Bool -> Int -> MonkeyState -> MonkeyState
getRound _ _ 0 ms = ms
getRound modulus disc n ms = let
    ms' = monkeyRound modulus disc ms
  in
    ms' `seq` getRound modulus disc (pred n) ms'

activity :: MonkeyState -> Map Int Int
activity = fmap $ \(M _ _ _ _ _ _ c) -> c

monkeyBusiness :: MonkeyState -> Int
monkeyBusiness = product . take 2 . reverse . sort . elems . activity

main = do
  f <- readFile "day11.input.txt"
  let ms = parseMonkeys f
  let modulus = getModulus ms
  putStr "Part 1: "
  print $ monkeyBusiness $ getRound modulus True 20 ms
  putStr "Part 2: "
  print $ monkeyBusiness $ getRound modulus False 10000 ms
