sample :: String
sample = unlines [
    "A Y",
    "B X",
    "C Z"
  ]

data Rps = R | P | S
c2Rps :: Char -> Rps
c2Rps 'A' = R
c2Rps 'B' = P
c2Rps 'X' = R
c2Rps 'Y' = P
c2Rps 'C' = S
c2Rps 'Z' = S

c2Wld :: Char -> Wld
c2Wld 'X' = L
c2Wld 'Y' = D
c2Wld 'Z' = W

rps2s :: Rps -> Int
rps2s R = 1
rps2s P = 2
rps2s S = 3

data Wld = W | L | D

outcome :: Rps -> Rps -> Wld
outcome R R = D
outcome R P = W
outcome R S = L
outcome P R = L
outcome P P = D
outcome P S = W
outcome S R = W
outcome S P = L
outcome S S = D

play :: Rps -> Wld -> Rps
play R W = P
play R L = S
play R D = R
play P W = S
play P L = R
play P D = P
play S W = R
play S L = P
play S D = S

wld2s :: Wld -> Int
wld2s W = 6
wld2s D = 3
wld2s L = 0

s2s1 :: String -> Int
s2s1 [o', ' ', p'] = (wld2s $ outcome o p) + rps2s p
  where
    o = c2Rps o'
    p = c2Rps p'

s2s2 :: String -> Int
s2s2 [o', ' ', wld'] = (wld2s $ outcome o p) + rps2s p
  where
    o = c2Rps o'
    wld = c2Wld wld'
    p = play o wld

f2s :: (String -> Int) -> String -> Int
f2s s2s = sum . map s2s . lines

main = do
  f <- readFile "day02.input.txt"
  putStr "Part 1: "
  print $ f2s s2s1 f
  putStr "Part 2: "
  print $ f2s s2s2 f