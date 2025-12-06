import Data.List (transpose)

sample :: [String]
sample = [
    "123 328  51 64 ",
    " 45 64  387 23 ",
    "  6 98  215 314",
    "*   +   *   +  "
  ]

parseRow :: String -> [Int]
parseRow = let
    p "" [] = []
    p n [] = [read $ reverse n]
    p "" (' ':cs) = p "" cs
    p n (' ':cs) = (read $ reverse n) : p "" cs
    p n (c:cs) = p (c:n) cs
  in p ""

parseDown :: [String] -> [([Int], Char)]
parseDown = let
    p rs [ops] = zip (transpose rs) $ filter (/= ' ') ops
    p rs (l:ls) = p ((parseRow l) : rs) ls
  in p []

parseRtl :: [String] -> [([Int], Char)]
parseRtl = let
    p o [] [] = []
    p o ns [] = [(ns, o)]
    p o ns (l:ls) =
      if all (==' ') l
      then p o ns ls
      else case l of
        (' ':n) -> p o ((read $ reverse n) : ns) ls
        (o':n) -> (ns, o) : (p o' [read $ reverse n] ls)
  in filter (not . null . fst) . p '?' [] . map reverse . transpose

eval :: ([Int], Char) -> Int
eval (ns, '*') = product ns
eval (ns, '+') = sum ns

main :: IO ()
main = do
  f <- lines <$> readFile "day06.input.txt"
  putStr "Part 1: "
  print $ sum $ map eval $ parseDown f
  putStr "Part 2: "
  print $ sum $ map eval $ parseRtl f 