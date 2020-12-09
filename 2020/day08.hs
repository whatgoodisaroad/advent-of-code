import Data.List (find)

lineToI :: String -> (String, Int)
lineToI s = (take 3 s, sign * (read $ drop 5 s))
  where
    sign = if s !! 4 == '+' then 1 else -1

exec :: [Int] -> [(String, Int)] -> Int -> Int -> (Int, Bool)
exec visited is pc acc
  | pc >= length is = (acc, True)
  | pc `elem` visited = (acc, False)
  | otherwise = case is !! pc of
      ("nop", _) -> exec (pc:visited) is (succ pc) acc
      ("acc", n) -> exec (pc:visited) is (succ pc) (acc + n)
      ("jmp", n) -> exec (pc:visited) is (pc + n) acc

replace :: [a] -> Int -> a -> [a]
replace l p v = take p l ++ [v] ++ drop (succ p) l

trySub :: [(String, Int)] -> String -> Int -> Maybe Int
trySub is op p = if good then return acc else Nothing
  where
    n = snd $ is !! p
    is' = replace is p (op, n)
    (acc, good) = exec [] is' 0 0

getInstructionPositionsByOp :: String -> [(String, Int)] -> [Int]
getInstructionPositionsByOp op = map fst . filter ((==op).fst.snd) . zip [0..]

trySubs :: String -> String -> [(String, Int)] -> Maybe Int
trySubs fromOp toOp is = do
  let accs  = map (\(Just a) -> a)
            $ filter (/= Nothing)
            $ map (trySub is toOp)
            $ getInstructionPositionsByOp fromOp is
  if null accs then Nothing else return $ head accs

tryBoth :: [(String, Int)] -> Maybe Int
tryBoth is = case (trySubs "jmp" "nop" is, trySubs "nop" "jmp" is) of
  (Just i, _) -> Just i
  (_, Just i) -> Just i
  _ -> Nothing

example = [
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6"
  ]

main = do
  f <- readFile "day08.input.txt"
  let is = map lineToI $ lines f
  putStr "Part 1: "
  print $ fst $ exec [] is 0 0
  putStr "Part 2: "
  print $ tryBoth is