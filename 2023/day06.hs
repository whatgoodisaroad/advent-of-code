sample :: [String]
sample = [
    "Time:      7  15   30",
    "Distance:  9  40  200"
  ]

input :: [String]
input = [
    "Time:        48     98     90     83",
    "Distance:   390   1103   1112   1360"
  ]

parse1 :: [String] -> [(Int, Int)]
parse1 [t, d] = times `zip` distances
  where
    times = map read $ words $ drop (length "Time:") t
    distances = map read $ words $ drop (length "Distance:") d

parse2 :: [String] -> (Int, Int)
parse2 [t, d] = (times, distances)
  where
    times = read $ concat $ words $ drop (length "Time:") t
    distances = read $ concat $ words $ drop (length "Distance:") d

dist :: Int -> Int -> Int
dist t h = h * (t - h)

win :: Int -> Int -> [Int]
win t r = filter (\h -> dist t h > r) [0..t]

main :: IO ()
main = do
  putStr "Part 1: "
  print $ product $ map (length . uncurry win) $ parse1 input
  putStr "Part 2: "
  print $ length $ uncurry win $ parse2 input