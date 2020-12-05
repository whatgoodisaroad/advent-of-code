import Text.ParserCombinators.Parsec

data Pass = Pass String Char Int Int deriving Show

passP :: GenParser Char st Pass
passP = do
  low <- fmap (read :: String -> Int) $ many digit
  char '-'
  high <- fmap (read :: String -> Int) $ many digit
  space
  sym <- oneOf ['a'..'z']
  char ':'
  space
  pass <- many $ oneOf ['a'..'z']
  char '\n'
  return $ Pass pass sym low high

passFileP :: GenParser Char st [Pass]
passFileP = do
  passwords <- many passP
  eof
  return passwords

isValidPart1 :: Pass -> Bool
isValidPart1 (Pass p s l h) = c >= l && c <= h
  where
    c = length $ filter (==s) p

isValidPart2 :: Pass -> Bool
isValidPart2 (Pass p s l h) = lowSet && not highSet || highSet && not lowSet
  where
    lowSet = p !! (pred l) == s
    highSet = p !! (pred h) == s

main = do
  db <- readFile "day02.input.txt"
  case parse passFileP "failed" db of
    Left err -> print "oops"
    Right passwords -> do
      putStr "Part 1: "
      print $ length $ filter isValidPart1 passwords
      putStr "Part 2: "
      print $ length $ filter isValidPart2 passwords
