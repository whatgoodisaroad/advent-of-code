import Control.Monad (guard)
import Data.List (find, findIndex, sortBy)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Control.Applicative (Alternative)

type Plan = [String]
type Pos = (Int, Int)
type Path = [Pos]
type Area = [Pos]

leadsTo :: Plan -> Pos -> Pos -> Bool
leadsTo m (r1, c1) (r2, c2) = (r2, c2) `elem` adjP m r1 c1

guardBounds :: Alternative f => Plan -> Pos -> Pos -> f ()
guardBounds m (r, c) (r', c')
  = guard
  $ r' >= 0
  && r' < length m
  && c' >= 0
  && c' < length (head m)
  && (r /= r' || c /= c')

adj :: Plan -> (Pos -> Bool) -> Pos -> [Pos]
adj m f (r, c) = do
  r' <- [pred r .. succ r]
  c' <- [pred c .. succ c]
  guardBounds m (r, c) (r', c')
  guard $ f (r', c')
  return (r', c')

adjP :: Plan -> Int -> Int -> [Pos]
adjP m r c = let
     x = (m !! r) !! c
   in if x == 'S'
     then adj m (\(r', c') -> leadsTo m (r', c') (r, c)) (r, c)
     else let
       v = length m
       h = length $ head m
       n = r > 0 && x `elem` "|LJ"
       s = r < pred v && x `elem` "|7F"
       w = c > 0 && x `elem` "-J7"
       e = c < pred h && x `elem` "-LF"
     in concat [
         [(pred r, c) | n],
         [(succ r, c) | s],
         [(r, pred c) | w],
         [(r, succ c) | e]
       ]

replaceS :: Plan -> Plan
replaceS m = do
  let Just (sr, sc) = findP (=='S') m
  let rmax = pred $ length m
  let cmax = pred $ length $ head m
  let up = sr > 0     && leadsTo m (pred sr, sc) (sr, sc)
  let dn = sr < rmax  && leadsTo m (succ sr, sc) (sr, sc)
  let lf = sc > 0     && leadsTo m (sr, pred sc) (sr, sc)
  let rt = sc < cmax  && leadsTo m (sr, succ sc) (sr, sc)
  r <- [0..rmax]
  let l = m !! r
  let char = case (up, dn, lf, rt) of {
      (True, True, _, _) -> '|';
      (True, _, True, _) -> 'J';
      (True, _, _, True) -> 'L';
      (_, True, True, _) -> '7';
      (_, True, _, True) -> 'F';
      (_, _, True, True) -> '-';
    }
  return $ if sr == r then take sc l ++ [char] ++ drop (succ sc) l else l

positions :: (Char -> Bool) -> Plan -> [Pos]
positions f m = do
  let rmax = length m
  let cmax = length $ head m
  r <- [0 .. pred rmax]
  c <- [0 .. pred cmax]
  guard $ f $ (m !! r) !! c
  return (r, c)

findP :: (Char -> Bool) -> Plan -> Maybe Pos
findP f m = listToMaybe $ positions f m

findPath :: Plan -> Path -> Path
findPath m ps@((r, c):_) = case find (`notElem` ps) $ adjP m r c of
  Nothing -> ps
  Just p' -> findPath m $ p':ps

polyfill :: Plan -> Pos -> Path -> Area
polyfill m (r, c) path = pf [(r, c)] [(r, c)]
  where
    pf :: [Pos] -> Area -> Area
    pf [] a = a
    pf (f:fs) area = pf (fs ++ f') (area ++ f')
      where
        f' = adj m (\p -> not $ p `elem` path || p `elem` area || p `elem` fs) f

enclosedLine :: [Int] -> String -> [Int]
enclosedLine pcols = cols 0 False
  where
    cols :: Int -> Bool -> String -> [Int]
    cols _ _ [] = []
    cols col intAbv (char:s) =
      let
        pchar = char `elem` "|-LJ7F"
        pcol = col `elem` pcols
        incl = if not pcol && intAbv then (col:) else id
        col' = succ col
        intAbv'
          | not pcol          = intAbv      -- Not in the path, no change
          | char == '|'       = not intAbv  -- Horizontal threshold
          | char `elem` "LJ"  = not intAbv  -- Toggle whether above is included
          | char `elem` "F7"  = intAbv      -- Above is not included
          | otherwise         = intAbv      -- Otherwise no change
      in
        incl $ cols col' intAbv' s

enclosedArea :: Plan -> Path -> Area
enclosedArea m p = do
  r <- [0 .. pred $ length m]
  let pr = map snd $ filter ((==r) . fst) p
  c <- enclosedLine pr $ m !! r
  return (r, c)

main :: IO ()
main = do
  m <- lines <$> readFile "day10.input.txt"
  let path = findPath m $ return $ fromJust $ findP (=='S') m
  putStr "Part 1: "
  print
    $ ceiling
    $ (/2)
    $ fromIntegral
    $ length path
  putStr "Part 2: "
  print $ length $ enclosedArea m path
  