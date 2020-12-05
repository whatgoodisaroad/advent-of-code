import Text.ParserCombinators.Parsec

landP :: GenParser Char st Bool
landP = fmap (=='#') $ oneOf ".#"

landLineP :: GenParser Char st [Bool]
landLineP = do
  ls <- many landP
  char '\n'
  return ls

fileP :: GenParser Char st [[Bool]]
fileP = do
  lls <- many landLineP
  eof
  return lls

isTree :: [[Bool]] -> Int -> Int -> Bool
isTree d row col = d !! row !! col'
  where
    w = length $ head d
    col' = col `mod` w

slopeTotal :: [[Bool]] -> (Int, Int) -> (Int, Int) -> Int -> Int
slopeTotal d (row, col) (rowD, colD) acc =
  if row >= length d
  then acc
  else let acc' = if isTree d row col then succ acc else acc in
    slopeTotal d (row + rowD, col + colD) (rowD, colD) acc'

main = do
  db <- readFile "day03.input.txt"
  case parse fileP "failed" db of
    Left err -> putStrLn $ show err
    Right d -> do
      putStr "Part 1: "
      print $ slopeTotal d (1, 3) (1, 3) 0
      putStr "Part 2: "
      let slopes = [(1,1),(1,3),(1,5),(1,7),(2,1)]
      flip mapM_ slopes $ \slope@(downD, rightD) -> do
        putStr $ "- Right " ++ show rightD ++ ", down " ++ show downD ++ ": "
        print $ slopeTotal d slope slope 0
      putStr "Prouct: "
      print $ product $ flip map slopes $ \slope -> slopeTotal d slope slope 0