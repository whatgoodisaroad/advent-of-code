import Data.List (elemIndex, nub, sort, sortBy, minimumBy)
import Data.Maybe (fromJust)

sample :: [String]
sample = [
    "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  ]

data HT = K5 | K4 | FH | K3 | P2 | P1 | HC deriving (Eq, Show)

instance Ord HT where
  t1 `compare` t2 = v t1 `compare` v t2
    where
      v :: HT -> Int
      v = fromJust . flip elemIndex [HC, P1, P2, K3, FH, K4, K5]

parse :: [String] -> [(String, Int)]
parse = map ((\[h, b] -> (h, read b)) . words)

k :: Char -> String -> Int
k c = length . filter (== c)

cardsInOrder :: String
cardsInOrder = "AKQJT98765432"

cardValue :: Bool -> Char -> Int
cardValue jokers
  = fromJust
  . flip elemIndex
    (reverse $ if jokers then "AKQT98765432J" else cardsInOrder)

categorize :: String -> HT
categorize h
  | all (== head h) $ tail h = K5
  | any (\c -> k c h == 4) h = K4
  | length u == 2 && ([2, 3] == sort (map (`k` h) u)) = FH
  | any (\c -> k c h == 3) h = K3
  | length u == 3 && length (filter (\c -> k c h == 2) u) == 2 = P2
  | any (\c -> k c h == 2) h = P1
  | otherwise = HC
  where
    u = nub h

compareHands :: Bool -> String -> String -> Ordering
compareHands jokers h1 h2
  = case categorize h2' `compare` categorize h1' of EQ -> hc h1 h2; o -> o
  where
    hc :: String -> String -> Ordering
    hc [] _ = EQ
    hc (c1:h1) (c2:h2)
      = case cardValue jokers c2 `compare` cardValue jokers c1 of
        EQ -> hc h1 h2
        o -> o
    h1' = if jokers then strengthen h1 else h1
    h2' = if jokers then strengthen h2 else h2

expand :: String -> [String]
expand [] = [[]]
expand (c:h) = do
  c' <- if c == 'J' then filter (/= 'J') cardsInOrder else [c]
  h' <- expand h
  return $ c' : h'

strengthen :: String -> String
strengthen = minimumBy (compareHands True) . expand

sortBids :: Bool -> [(String, Int)] -> [(String, Int)]
sortBids jokers = sortBy (\(h1, _) (h2, _) -> compareHands jokers h2 h1)

winnings :: Bool -> [(String, Int)] -> Int
winnings jokers
  = sum
  . zipWith (curry (\(r, (_, b)) -> b * r)) [1..]
  . sortBids jokers

main :: IO ()
main = do
  f <- lines <$> readFile "day07.input.txt"
  let bids = parse f
  putStr "Part 1: "
  print $ winnings False bids
  putStr "Part 2: "
  print $ winnings True bids