import Data.List (nub)
import Data.Map.Strict as Map (Map, fromList, keys, lookup)
import Data.Maybe (fromJust)

startsWith :: String -> String -> Bool
startsWith s p = p == take (length p) s

endsWith :: String -> String -> Bool
endsWith s e = startsWith (reverse s) (reverse e)

splitBy :: String -> String -> [String]
splitBy = splitBy' ""

splitBy' :: String -> String -> String -> [String]
splitBy' b _ [] = [reverse b]
splitBy' b p s@(c:cs) =
  if startsWith s p
  then (reverse b) : (splitBy' "" p (drop (length p) s))
  else splitBy' (c:b) p cs

trimL :: String -> String
trimL (' ':s) = trimL s
trimL s = s

trimR :: String -> String
trimR = reverse . trimL . reverse

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace f t (c:cs) =
  if c == f
  then t : replace f t cs
  else c : replace f t cs

toPairs :: String -> [(String, String)]
toPairs
  = map (\[k,v] -> (k,v))
  . map (splitBy ":")
  . splitBy " "

requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
keyPasslist = requiredKeys ++ ["cid"]

isValidPart1 :: Map String String -> Bool
isValidPart1 m = hasRequired && noInvalid
  where
    ks = keys m
    hasRequired = all (`elem` ks) requiredKeys
    noInvalid = all (`elem` keyPasslist) ks

isValidPart2 :: Map String String -> Bool
isValidPart2 m = and [
    isValidPart1 m,
    allDigits byr && isIntInRange byr 1920 2002,
    allDigits iyr && isIntInRange iyr 2010 2020,
    allDigits eyr && isIntInRange eyr 2020 2030,
    or [
      hgt `endsWith` "cm" && allDigits hgtNum && isIntInRange hgtNum 150 193,
      hgt `endsWith` "in" && allDigits hgtNum && isIntInRange hgtNum 59 76
    ],
    length hcl == 7 && hcl `startsWith` "#" && (allHex $ tail hcl),
    ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
    length pid == 9 && allDigits pid
  ]
  where
    get = fromJust . flip Map.lookup m
    byr = get "byr"
    iyr = get "iyr"
    eyr = get "eyr"
    hgt = get "hgt"
    hcl = get "hcl"
    ecl = get "ecl"
    pid = get "pid"
    hgtNum = init $ init hgt
    allDigits = all $ flip elem ['0'..'9']
    allHex = all $ flip elem $ ['0'..'9'] ++ ['a'..'f']

isIntInRange :: String -> Int -> Int -> Bool
isIntInRange s l h = i >= l && i <= h
  where
    i = (read :: String -> Int) s

noRepeats :: [(String, String)] -> Bool
noRepeats ps = (map fst ps) == (nub $ map fst ps)

main = do
  f <- readFile "day04.input.txt"
  putStr "Part 1: "
  print
    $ length
    $ filter isValidPart1
    $ map fromList
    $ map toPairs
    $ map trimR
    $ map (replace '\n' ' ')
    $ splitBy "\n\n" f
  putStr "Part 2: "
  print
    $ length
    $ filter isValidPart2
    $ map fromList
    $ filter noRepeats
    $ map toPairs
    $ map trimR
    $ map (replace '\n' ' ')
    $ splitBy "\n\n" f
