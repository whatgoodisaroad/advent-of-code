data Instruction = Noop | Addx Int deriving Show

parse :: [String] -> [Instruction]
parse [] = []
parse ("noop":is) = Noop : parse is
parse (addxv:is) = Addx (read $ drop 5 addxv) : parse is

exec :: Int -> [Instruction] -> [Int]
exec x [] = [x]
exec x (Noop:is) = x : exec x is
exec x (Addx v:is) = let x' = x + v in x : x : exec x' is

strength :: [Int] -> Int -> Int
strength sequence cycle = cycle * (sequence !! pred cycle)

lit :: [Int] -> Int -> Bool
lit sequence cycle = (abs $ (cycle `mod` 40) - (sequence !! cycle)) < 2

line :: [Int] -> Int -> Int -> String
line sequence from to = do
  cycle <- [from .. to]
  return $ if lit sequence (pred cycle) then '#' else '.'

main = do
  f <- readFile "day10.input.txt"
  let p = parse $ lines f
  let sequence = exec 1 p
  putStr "Part 1: "
  print $ sum $ map (strength sequence) [
      20,
      60,
      100,
      140,
      180,
      220
    ]
  putStrLn "Part 2:"
  mapM_ (putStrLn . (uncurry $ line sequence)) [
      (  1,  40),
      ( 41,  80),
      ( 81, 120),
      (121, 160),
      (161, 200),
      (201, 240)
    ]
