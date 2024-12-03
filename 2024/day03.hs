import Control.Monad.Identity (Identity)
import Text.Parsec (ParsecT)
import Text.ParserCombinators.Parsec (
    ParseError,
    anyChar,
    char,
    digit,
    many,
    many1,
    parse,
    try,
    string,
    (<|>)
  )

sample1 :: String
sample1
  = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

sample2 :: String
sample2
  = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

data Instruction = Mul Int Int | Do | Dont deriving (Eq, Show)

intP :: ParsecT String u Identity Int
intP = read <$> many1 digit

mulP :: ParsecT String u Identity Instruction
mulP = do
  string "mul("
  l <- intP
  char ','
  r <- intP
  char ')'
  return $ Mul l r

instrP :: ParsecT String u Identity Instruction
instrP
  = (try $ mulP)
  <|> (try $ string "do()" >> return Do)
  <|> (try $ string "don't()" >> return Dont)

memP :: ParsecT String u Identity [Instruction]
memP
  = fmap concat
  $ many
  $ (try $ instrP >>= return . return) <|> (anyChar >> return [])

parseMemory :: String -> [Instruction]
parseMemory s = let (Right ps) = parse memP "fail" s in ps

part1 :: [Instruction] -> Int
part1 is = sum $ flip map is $ \i -> case i of
  (Mul l r) -> l * r
  _ -> 0

part2 :: [Instruction] -> Int
part2 [] = 0
part2 (Dont:is) = part2 $ dropWhile (/= Do) is
part2 ((Mul l r):is) = (l * r) + part2 is 
part2 (_:is) = part2 is

main = do
  f <- readFile "day03.input.txt"
  putStr "Part 1: "
  print $ part1 $ parseMemory f
  putStr "Part 2: "
  print $ part2 $ parseMemory f