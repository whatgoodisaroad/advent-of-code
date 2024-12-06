import Control.Monad (guard)
import Data.List (nub)

sample :: [String]
sample = [
    "....#.....",
    ".........#",
    "..........",
    "..#.......",
    ".......#..",
    "..........",
    ".#..^.....",
    "........#.",
    "#.........",
    "......#..."
  ]

type Pos = (Int, Int)
type State = (Int, Int, [Pos], Pos, Char)

parse :: [String] -> State
parse g = (rows, cols, obstacles, pos, dir)
  where
    rows = length g
    cols = length $ g !! 0
    obstacles = do
      row <- [0 .. pred rows]
      col <- [0 .. pred cols]
      guard $ g !! row !! col == '#'
      return (row, col)
    pos = (!!0) $ do
      row <- [0 .. pred rows]
      col <- [0 .. pred cols]
      guard $ elem (g !! row !! col) "v<^>"
      return (row, col)
    dir = g !! fst pos !! snd pos

path :: State -> [State]
path s@(rs, cs, os, (row, col), dir)
  -- Check edges
  | dir == '^' && row == -1 = []
  | dir == '<' && col == -1 = []
  | dir == 'v' && row == rs = []
  | dir == '>' && col == cs = []
  -- Check obstacles
  | elem pos' os = path (rs, cs, os, (row, col), dir')
  -- Move
  | otherwise = s : path (rs, cs, os, pos', dir)
  where
    pos' = case dir of
      '^' -> (pred row, col)
      '>' -> (row, succ col)
      'v' -> (succ row, col)
      '<' -> (row, pred col)
    dir' = case dir of
      '^' -> '>'
      '>' -> 'v'
      'v' -> '<'
      '<' -> '^'

getPos :: State -> Pos
getPos (_, _, _, p, _) = p

part1 :: State -> Int
part1 = length . nub . map getPos . path

isLoop :: [State] -> Bool
isLoop = l [] . map (\(_, _, _, p, d) -> (p, d))
  where
    l :: [(Pos, Char)] -> [(Pos, Char)] -> Bool
    l v [] = False
    l v (s:ss) = if elem s v then True else l (s:v) ss

part2 :: State -> Int
part2 (rs, cs, os, pos, dir)
  = length
  $ filter (\o -> isLoop $ path (rs, cs, o:os, pos, dir))
  $ nub
  $ map getPos
  $ path (rs, cs, os, pos, dir)

main = do
  f <- lines <$> readFile "day06.input.txt"
  let state = parse f
  putStr "Part 1: "
  print $ part1 state
  putStr "Part 2: "
  print $ part2 state
