lineToCmd :: String -> (Char, Int)
lineToCmd (c:cs) = (c, read cs)

rotateCW, rotateCCW :: Int -> Int -> Int
rotateCW dir deg = (dir + (deg `div` 90)) `mod` 4
rotateCCW dir deg = rotateCW dir (360 - deg)

rotatePointCW90 :: Int -> Int -> (Int, Int)
rotatePointCW90 row col = (col, -row)

rotatePointCW, rotatePointCCW :: Int -> Int -> Int -> (Int, Int)
rotatePointCW row col deg
  | deg `div` 90 <= 0 = (row, col)
  | otherwise = rotatePointCW row' col' (deg - 90)
  where
    (row', col') = rotatePointCW90 row col
rotatePointCCW row col deg = rotatePointCW row col (360 - deg)

move1 :: (Int, Int, Int) -> (Char, Int) -> (Int, Int, Int)
move1 (dir, row, col)  ('N', dist) = (dir, row - dist, col)
move1 (dir, row, col)  ('S', dist) = (dir, row + dist, col)
move1 (dir, row, col)  ('W', dist) = (dir, row, col - dist)
move1 (dir, row, col)  ('E', dist) = (dir, row, col + dist)
move1 (dir, row, col)  ('R', deg)  = (rotateCW dir deg, row, col)
move1 (dir, row, col)  ('L', deg)  = (rotateCCW dir deg, row, col)
move1 (0, row, col)    ('F', dist) = (0, row - dist, col)
move1 (1, row, col)    ('F', dist) = (1, row, col + dist)
move1 (2, row, col)    ('F', dist) = (2, row + dist, col)
move1 (3, row, col)    ('F', dist) = (3, row, col - dist)

move2 :: (Int, Int, Int, Int) -> (Char, Int) -> (Int, Int, Int, Int)
move2 (row, col, rowD, colD) ('N', dist) = (row, col, rowD - dist, colD)
move2 (row, col, rowD, colD) ('S', dist) = (row, col, rowD + dist, colD)
move2 (row, col, rowD, colD) ('W', dist) = (row, col, rowD, colD - dist)
move2 (row, col, rowD, colD) ('E', dist) = (row, col, rowD, colD + dist)
move2 (row, col, rowD, colD) ('R', deg)  = (row, col, rowD', colD')
  where (rowD', colD') = rotatePointCW rowD colD deg
move2 (row, col, rowD, colD) ('L', deg)  = (row, col, rowD', colD')
  where (rowD', colD') = rotatePointCCW rowD colD deg
move2 (row, col, rowD, colD) ('F', dist) = (
    row + dist * rowD,
    col + dist * colD,
    rowD,
    colD
  )

manhattanDist1 :: (Int, Int, Int) -> Int
manhattanDist1 (_, row, col) = abs row + abs col
manhattanDist2 :: (Int, Int, Int, Int) -> Int
manhattanDist2 (row, col, _, _) = abs row + abs col

example = [
    "F10",
    "N3",
    "F7",
    "R90",
    "F11"
  ]

main = do
  f <- readFile "day12.input.txt"
  let cmds = map lineToCmd $ lines f
  putStr "Part 1: "
  print $ manhattanDist1 $ foldl move1 (1, 0, 0) cmds
  putStr "Part 2: "
  print $ manhattanDist2 $ foldl move2 (0, 0, -1, 10) cmds