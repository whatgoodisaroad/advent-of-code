sample :: [String]
sample = [
    "L68",
    "L30",
    "R48",
    "L5",
    "R60",
    "L55",
    "L1",
    "L99",
    "R14",
    "L82"
  ]

modulus :: Int
modulus = 100

positions :: Bool -> Int -> [String] -> [Int]
positions _ _ [] = []
positions clicks n (i:is) = let
    (o, d) = case i of
      ('L':ds) -> (read ds, -1)
      ('R':ds) -> (read ds, 1)
    n' o d = mod (n + o * d) modulus
    cs = if not clicks
      then [n' o d]
      else map (\o -> n' o d) $ take o $ enumFrom 0
  in cs ++ positions clicks (n' o d) is

main :: IO ()
main = do
  f <- lines <$> readFile "day01.input.txt"
  putStr "Part 1: "
  print $ length $ filter (==0) $ positions False 50 f
  putStr "Part 2: "
  print $ length $ filter (==0) $ positions True 50 f