import Data.List (sort)
import Data.Map.Strict (Map, (!), adjust, empty, insertWith, keys)
import Text.ParserCombinators.Parsec

sample :: String
sample = unlines [
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
  ]

type Stacks = Map Int String
type Move = (Int, Int, Int)

parseStacks :: [String] -> Stacks
parseStacks = foldr (\l m -> parseStackLine m 1 l) empty

parseStackLine :: Stacks -> Int -> String -> Stacks
parseStackLine m _ [] = m
parseStackLine m idx (' ':' ':' ':' ':s) = parseStackLine m (succ idx) s
parseStackLine m idx ('[': n :']':s) = parseStackLine (insertWith (++) idx [n] m) (succ idx) s
parseStackLine m idx (' ':s) = parseStackLine m idx s

intP :: GenParser Char st Int
intP = fmap read $ many1 digit

lineP :: GenParser Char st Move
lineP = do
  string "move "
  qty <- intP
  string " from "
  from <- intP
  string " to "
  to <- intP
  return (qty, from, to)

splitLines :: [String] -> [String] -> ([String], [String])
splitLines stacks ("":ls) = (reverse stacks, ls)
splitLines stacks ((' ':'1':_):ls) = splitLines stacks ls
splitLines stacks (l:ls) = splitLines (l:stacks) ls

parseFile :: String -> (Stacks, [Move])
parseFile f = let
    (stacksF, movesF) = splitLines [] $ lines f
    Right moves
      = mapM (parse (lineP) "failed")
      $ filter (not.null)
      $ movesF
  in
    (parseStacks stacksF, moves)

move :: Bool -> Move -> Stacks -> Stacks
move preserve (qty, from, to) s = let
    stack = take qty $ s ! from
    stack' = if preserve then stack else reverse stack
    s' = adjust (drop qty) from s
    s'' = adjust (stack' ++) to s'
  in
    s''

moveAll :: Bool -> Stacks -> [Move] -> Stacks
moveAll _ s [] = s
moveAll p s (m:ms) = moveAll p (move p m s) ms

tops :: Stacks -> String
tops s = concatMap (\k -> take 1 $ s ! k) $ sort $ keys s

main = do
  f <- readFile "day05.input.txt"
  let (stacks, moves) = parseFile f
  putStr "Part 1: "
  putStrLn $ tops $ moveAll False stacks moves
  putStr "Part 2: "
  putStrLn $ tops $ moveAll True stacks moves