import Control.Monad.Identity (Identity)
import Text.Parsec (ParsecT)
import Text.ParserCombinators.Parsec (
    ParseError,
    char,
    choice,
    digit,
    eof,
    many,
    many1,
    sepBy,
    sepBy1,
    spaces,
    parse,
    try,
    string
  )

sample :: [String]
sample = [
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  ]

data RGB = RGB Int Int Int deriving Show
data Game = Game Int [RGB] deriving Show
type Parser u t = ParsecT String u Identity t

gameId :: Game -> Int
gameId (Game id _) = id

rounds :: Game -> [RGB]
rounds (Game _ rs) = rs

intP :: Parser u Int
intP = read <$> many1 digit

gameParser :: Parser u Game
gameParser = try $ do
  string "Game "
  num <- intP
  string ": "
  rounds <- rgbParser `sepBy1` string "; "
  spaces
  return $ Game num rounds

rgbParser :: Parser u RGB
rgbParser = pairsToRgb <$> colorParser `sepBy1` string ", "

colorParser :: Parser u (Int, String)
colorParser = do
  qty <- intP
  char ' '
  color <- choice $ map string ["red", "blue", "green"]
  return (qty, color)

pairsToRgb :: [(Int, String)] -> RGB
pairsToRgb [] = RGB 0 0 0
pairsToRgb ((qty, color):ps) = RGB r' g' b'
  where
    (RGB r g b) = pairsToRgb ps
    r' = if color == "red"    then r + qty else r
    g' = if color == "green"  then g + qty else g
    b' = if color == "blue"   then b + qty else b

parseFile :: String -> Either ParseError [Game]
parseFile = parse (many gameParser) "fail"

isPossible :: RGB -> Bool
isPossible (RGB r g b) = r <= 12 && g <= 13 && b <= 14

minColors :: Game -> RGB
minColors = foldr f (RGB 0 0 0) . rounds
  where
    f (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 `max` r2) (g1 `max` g2) (b1 `max` b2)

power :: RGB -> Int
power (RGB r g b) = r * b * g

main = do
  (Right games) <- parseFile <$> readFile "day02.input.txt"
  putStr "Part 1: "
  print $ sum $ map gameId $ filter (all isPossible . rounds) games
  putStr "Part 2: "
  print $ sum $ map (power . minColors) $ games
