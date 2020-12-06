import Data.Map.Strict (Map, empty, insert, toList, (!))

splitPgm :: String -> [Int]
splitPgm = sp []
  where
    sp [] [] = []
    sp b [] = [read $ reverse b]
    sp b (i:is) =
      if i == ','
      then (read $ reverse b) : (sp [] is)
      else sp (i:b) is

toMap :: [Int] -> Map Int Int
toMap = foldr (uncurry insert) empty . zip [0..]

exec :: Int -> Map Int Int -> Map Int Int
exec pc pgm = case pgm ! pc of
  99 -> pgm
  1 -> exec pc' $ insert dest (opL + opR) pgm
  2 -> exec pc' $ insert dest (opL * opR) pgm
  where
    opL = pgm ! (pgm ! (pc + 1))
    opR = pgm ! (pgm ! (pc + 2))
    dest = pgm ! (pc + 3)
    pc' = pc + 4

repairProgram :: Map Int Int -> Map Int Int
repairProgram = modifyProgram 12 2

modifyProgram :: Int -> Int -> Map Int Int -> Map Int Int
modifyProgram p1 p2
  = insert 1 p1
  . insert 2 p2

target = 19690720

bruteForce :: Map Int Int -> [(Int, Int)]
bruteForce pgm = filter f [(p1, p2) | p1 <- [0..99], p2 <- [0..99]]
  where
    f (p1, p2) = target == (exec 0 $ modifyProgram p1 p2 pgm) ! 0


main = do
  f <- readFile "day02.input.txt"
  putStr "Part 1: "
  print
    $ (!0)
    $ exec 0
    $ repairProgram
    $ toMap
    $ splitPgm f
  putStr "Par 2: "
  print
    $ (\(n, v) -> 100 * n + v)
    $ head
    $ bruteForce
    $ toMap
    $ splitPgm f