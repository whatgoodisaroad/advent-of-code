import Data.Set (Set, empty, insert, size)

type Pos = (Int, Int)
type Rope = [Pos]
type State = ([Pos], Set Pos)
data Dir = U | D | R | L deriving (Read, Show)
type Sequence = [(Dir, Int)]

sample1, sample2 :: Sequence
sample1 = [(R, 4), (U, 4), (L, 3), (D, 1), (R, 4), (D, 1), (L, 5), (R, 2)]
sample2 = [(R, 5), (U, 8), (L, 8), (D, 3), (R, 17), (D, 10), (L, 25), (U, 20)]

parse :: String -> Sequence
parse = map pl . lines
  where
    pl :: String -> (Dir, Int)
    pl (dc:' ':qs) = (read [dc], read qs)

clamp :: Int -> Int
clamp x = if x < 0 then -1 else if x > 0 then 1 else 0

(+>) :: Pos -> Pos -> Pos
(tx, ty) +> (hx, hy) = let
    (dx, dy) = (hx - tx, hy - ty)
    (ax, ay) = (abs dx, abs dy)
  in
    if ax > 1 || ay > 1
      then (tx + clamp dx, ty + clamp dy)
      else (tx, ty)

(++>) :: Rope -> Pos -> Rope
[] ++> h = [h]
(t:ts) ++> h = h : (ts ++> (t +> h))

u :: Dir -> Pos -> Pos
u U (x, y) = (x, pred y)
u D (x, y) = (x, succ y)
u L (x, y) = (pred x, y)
u R (x, y) = (succ x, y)

m1 :: Dir -> State -> State
m1 d (h:ts, s) = let
    h' = u d h
    r = ts ++> h'
    t' = last r
    s' = insert t' s
  in
    (r, s')

mn :: Dir -> Int -> State -> State
mn _ 0 s = s
mn d n s = mn d (pred n) $ m1 d s

zero :: Int -> State
zero n = (take n $ repeat (0, 0), insert (0, 0) empty)

exec :: Sequence -> State -> State
exec [] s = s
exec ((d, q):ms) s = exec ms $ mn d q s

main = do
  f <- readFile "day09.input.txt"
  let s = parse f
  putStr "Part 1: "
  let (_, visited) = exec s $ zero 2
  print $ size visited
  putStr "Part 2: "
  let (_, visited) = exec s $ zero 10
  print $ size visited
