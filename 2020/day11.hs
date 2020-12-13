data Spot = Floor | Empty | Occupued deriving Eq

charToSpot :: Char -> Spot
charToSpot '.' = Floor
charToSpot 'L' = Empty
charToSpot '#' = Occupued

instance Show Spot where
  show Floor = "."
  show Empty = "L"
  show Occupued = "#"

showLayout :: [[Spot]] -> String
showLayout = unlines . map (concatMap show)

rowToSpots :: String -> [Spot]
rowToSpots = map charToSpot

layoutSize :: [[Spot]] -> (Int, Int)
layoutSize layout = (length layout, length $ head layout)

numOccupied :: [[Spot]] -> Int
numOccupied = sum . map (length . filter (==Occupued))

visibleInDir :: [[Spot]] -> Int -> Int -> Int -> Int -> Bool
visibleInDir layout row col rowD colD =
  if oob then False else case (layout !! (row + rowD)) !! (col + colD) of
    Occupued -> True
    Empty -> False
    _ -> visibleInDir layout (row + rowD) (col + colD) rowD colD
  where
    (sizeR, sizeC) = layoutSize layout
    oob = or [
        row == 0          && rowD < 0,
        col == 0          && colD < 0,
        row == pred sizeR && rowD > 0,
        col == pred sizeC && colD > 0
      ]

nextStatePart1 :: [[Spot]] -> Int -> Int -> Spot
nextStatePart1 layout row col = case (layout !! row) !! col of
  Floor     -> Floor
  Empty     -> if adjacentOccupied == 0 then Occupued else Empty
  Occupued  -> if adjacentOccupied >= 4 then Empty    else Occupued
  where
    (sizeR, sizeC) = layoutSize layout
    adjacent = do
      r <- [ (max 0 $ pred row) .. (min (pred sizeR) $ succ row)]
      c <- [ (max 0 $ pred col) .. (min (pred sizeC) $ succ col)]
      if r == row && c == col then [] else return $ (layout !! r) !! c
    adjacentOccupied = length $ filter (==Occupued) adjacent

nextStatePart2 :: [[Spot]] -> Int -> Int -> Spot
nextStatePart2 layout row col = case (layout !! row) !! col of
  Floor     -> Floor
  Empty     -> if adjacentOccupied == 0 then Occupued else Empty
  Occupued  -> if adjacentOccupied >= 5 then Empty    else Occupued
  where
    (sizeR, sizeC) = layoutSize layout
    adjacentOccupied
      = length
      $ filter id
      $ map (uncurry $ visibleInDir layout row col)
      $ do
        rowD <- [-1..1]
        colD <- [-1..1]
        if rowD == 0 && colD == 0
        then []
        else return (rowD, colD)

nextLayout :: ([[Spot]] -> Int -> Int -> Spot) -> [[Spot]] -> [[Spot]]
nextLayout fn layout = do
  row <- [ 0 .. pred sizeR ]
  return $ nextRow layout row
  where
    (sizeR, sizeC) = layoutSize layout
    nextRow :: [[Spot]] -> Int -> [Spot]
    nextRow layout row = do
      col <- [ 0 .. pred sizeC ]
      return $ fn layout row col

untilStable :: ([[Spot]] -> Int -> Int -> Spot) -> [[Spot]] -> [[Spot]]
untilStable fn layout =
  if layout == layout'
  then layout
  else untilStable fn layout'
  where
    layout' = nextLayout fn layout

example = [
    "L.LL.LL.LL",
    "LLLLLLL.LL",
    "L.L.L..L..",
    "LLLL.LL.LL",
    "L.LL.LL.LL",
    "L.LLLLL.LL",
    "..L.L.....",
    "LLLLLLLLLL",
    "L.LLLLLL.L",
    "L.LLLLL.LL"
  ]

main = do
  f <- readFile "day11.input.txt"
  let layout = map rowToSpots $ lines f
  putStr "Part 1: "
  print $ numOccupied $ untilStable nextStatePart1 layout
  putStr "Part 2: "
  print $ numOccupied $ untilStable nextStatePart2 layout

