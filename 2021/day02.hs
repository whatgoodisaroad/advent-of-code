import Data.List (isPrefixOf)

examplePlan = [
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  ]

data Command = Forward Int | Down Int | Up Int deriving Show

parsePlan :: [String] -> [Command]
parsePlan = map parseCommand

parseCommand :: String -> Command
parseCommand s
  | "forward " `isPrefixOf` s = Forward $ read $ drop (length "forward ") s
  | "down " `isPrefixOf` s = Down $ read $ drop (length "down ") s
  | "up " `isPrefixOf` s = Up $ read $ drop (length "up ") s

type Position = (Int, Int, Int)

navigate1, navigate2 :: Position -> [Command] -> Position

navigate1 orig [] = orig
navigate1 (h, d, a) ((Forward n):cs) = navigate1 (h + n, d, a) cs
navigate1 (h, d, a) ((Down n):cs) = navigate1 (h, d + n, a) cs
navigate1 (h, d, a) ((Up n):cs) = navigate1 (h, d - n, a) cs

navigate2 orig [] = orig
navigate2 (h, d, a) ((Forward n):cs) = navigate2 (h + n, d + n * a, a) cs
navigate2 (h, d, a) ((Down n):cs) = navigate2 (h, d, a + n) cs
navigate2 (h, d, a) ((Up n):cs) = navigate2 (h, d, a - n) cs

main = do
  f <- readFile "day02.input.txt"
  let plan = parsePlan $ lines f
  putStr "Part 1: "
  let (h, d, _) = navigate1 (0, 0, 0) plan
  print $ h * d
  putStr "Part 2: "
  let (h', d', _) = navigate2 (0, 0, 0) plan
  print $ h' * d'