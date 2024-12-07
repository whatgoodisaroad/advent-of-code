sample :: [String]
sample = [
    "190: 10 19",
    "3267: 81 40 27",
    "83: 17 5",
    "156: 15 6",
    "7290: 6 8 6 15",
    "161011: 16 10 13",
    "192: 17 8 14",
    "21037: 9 7 18 13",
    "292: 11 6 16 20"
  ]

data Term = Val Int | Add | Mul | Cat deriving (Eq, Show)
type Spec = (Int, [Int])
type Equation = (Int, [Term])

parse :: [String] -> [Spec]
parse [] = []
parse (s:ss) = let (lhs:rhs) = words s in
  (read $ init lhs, map read rhs) : parse ss

operators :: [Term] -> Int -> [[Term]]
operators ops 0 = [[]]
operators ops n = do
  o <- ops
  os <- operators ops (pred n)
  return $ o:os

interleave :: [a] -> [a] -> [a]
interleave l [] = l
interleave [] r = r
interleave (l:ls) (r:rs) = l:r:interleave ls rs

fill :: [Term] -> Spec -> [Equation]
fill ops (lhs, rhs) = do
  let terms = map Val rhs
  ops <- operators ops $ pred $ length terms
  return $ (lhs, interleave terms ops)

evalRtl :: Int -> [Term] -> Maybe Int
evalRtl limit [Val n] = if n > limit then Nothing else Just n
evalRtl limit (Val v : op : terms) = do
  rhs <- evalRtl limit terms
  let n = case op of {
      Add -> v + rhs;
      Mul -> v * rhs;
      Cat -> read $ show rhs ++ show v;
    }
  if n > limit
    then Nothing
    else Just n

valid :: Equation -> Bool
valid (lhs, rhs) = case evalRtl lhs $ reverse rhs of
  Just n -> lhs == n
  Nothing -> False

possible :: [Term] -> Spec -> Bool
possible ops s = or $ fill ops s >>= return . valid

part1 :: [Spec] -> Int
part1 = sum . map fst . filter (possible [Add, Mul])

part2 :: [Spec] -> Int
part2 = sum . map fst . filter (possible [Add, Mul, Cat])

main = do
  f <- lines <$> readFile "day07.input.txt"
  let spec = parse f
  putStr "Part 1: "
  print $ part1 spec
  putStr "Part 2: "
  print $ part2 spec
