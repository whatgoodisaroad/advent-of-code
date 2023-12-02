import Data.Char (intToDigit, isDigit)
import Data.List (findIndex, isPrefixOf)

sample1 :: [String]
sample1 = [
    "1abc2",
    "pqr3stu8vwx",
    "a1b2c3d4e5f",
    "treb7uchet"
  ]

sample2 :: [String]
sample2 = [
  "two1nine",
  "eightwothree",
  "abcone2threexyz",
  "xtwone3four",
  "4nineeightseven2",
  "zoneight234",
  "7pqrstsixteen"
  ]

readDigits :: String -> Int
readDigits s = let ds = filter isDigit s in read [head ds, last ds]

numbers :: [String]
numbers = words "zero one two three four five six seven eight nine"

firstDigit :: [String] -> String -> Char
firstDigit ws s@(c:s') =
  if isDigit c then c
  else maybe (firstDigit ws s') intToDigit (findIndex id $ map (`isPrefixOf` s) ws)

lastDigit :: [String] -> String -> Char
lastDigit ws s = firstDigit (map reverse ws) $ reverse s

readDigitsAndWords :: String -> Int
readDigitsAndWords s = read [firstDigit numbers s, lastDigit numbers s]

main = do
  f <- lines <$> readFile "day01.input.txt"
  putStr "Part 1: "
  print $ sum $ map readDigits f
  putStr "Part 2: "
  print $ sum $ map readDigitsAndWords f
