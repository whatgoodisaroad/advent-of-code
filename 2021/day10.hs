import Data.List (sort)

sample = [
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  ]

data Error = Valid | InvalideBrace Char | MissingBrace String
  deriving (Eq, Show)

findError :: String -> String -> Error
findError ('(':stack) (')':input) = findError stack input
findError ('[':stack) (']':input) = findError stack input
findError ('{':stack) ('}':input) = findError stack input
findError ('<':stack) ('>':input) = findError stack input
findError stack ('(':input) = findError ('(':stack) input
findError stack ('[':input) = findError ('[':stack) input
findError stack ('{':input) = findError ('{':stack) input
findError stack ('<':input) = findError ('<':stack) input
findError [] [] = Valid
findError s [] = MissingBrace $ reverse s
findError _ (c:_) = InvalideBrace c

scoreInvalid :: Error -> Int
scoreInvalid (InvalideBrace ')') = 3
scoreInvalid (InvalideBrace ']') = 57
scoreInvalid (InvalideBrace '}') = 1197
scoreInvalid (InvalideBrace '>') = 25137
scoreInvalid _ = 0

scoreMissing :: Error -> Int
scoreMissing (MissingBrace stack) = scoreStack stack
scoreMissing (_) = 0

scoreStack :: String -> Int
scoreStack [] = 0
scoreStack ('(':s) = 1 + 5 * scoreStack s
scoreStack ('[':s) = 2 + 5 * scoreStack s
scoreStack ('{':s) = 3 + 5 * scoreStack s
scoreStack ('<':s) = 4 + 5 * scoreStack s

median :: Ord a => [a] -> a
median l = sort l !! (div (pred $ length l) 2)

main = do
  f <- readFile "day10.input.txt"
  let system = lines f
  putStr "Part 1: "
  print $ sum $ map (scoreInvalid . findError []) system
  putStr "Part 2: "
  print $ median $ filter (>0) $ map (scoreMissing . findError []) system