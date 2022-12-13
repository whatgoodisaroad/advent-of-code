import Data.Char (digitToInt)

sample :: String
sample = unlines [
    "30373",
    "25512",
    "65332",
    "33549",
    "35390"
  ]

type M = [[Int]]

parse :: String -> M
parse = (map $ map digitToInt) . lines

visH, visV, vis :: Int -> Int -> M -> Bool
visH row col m = let
    hs = m !! row
    t = hs !! col
    l = take col hs
    r = drop (succ col) hs
  in
    all (<t) l || all (<t) r
visV row col m = let
    vs = map (!! col) m
    t = vs !! row
    a = take row vs
    b = drop (succ row) vs
  in
    all (<t) a || all (<t) b
vis row col m = visH row col m || visV row col m

dist :: Int -> [Int] -> Int
dist _ [] = 0
dist t' (t:ts) = if t < t' then succ $ dist t' ts else 1

scoreH, scoreV :: Int -> Int -> M -> (Int, Int)
scoreH row col m = let
    hs = m !! row
    t = hs !! col
    l = reverse $ take col hs
    r = drop (succ col) hs
  in
    (dist t l, dist t r)
scoreV row col m = let
    vs = map (!! col) m
    t = vs !! row
    a = reverse $ take row vs
    b = drop (succ row) vs
  in
    (dist t a, dist t b)
score :: Int -> Int -> M -> (Int, Int, Int, Int)
score row col m = let
    (l, r) = scoreH row col m
    (a, b) = scoreV row col m
  in
    (a, l, r, b)
scoreP :: Int -> Int -> M -> Int
scoreP row col m = let
    (a, l, r, b) = score row col m
  in
    a * l * r * b

main = do
  f <- readFile "day08.input.txt"
  let m = parse f
  putStr "Part 1: "
  print $ length $ filter id $ do
    row <- [0 .. (pred $ length m)]
    col <- [0 .. (pred $ length $ m !! row)]
    return $ vis row col m
  putStr "Part 2: "
  print $ maximum $ do
    row <- [0 .. (pred $ length m)]
    col <- [0 .. (pred $ length $ m !! row)]
    return $ scoreP row col m
