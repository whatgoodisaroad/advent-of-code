import Data.List (findIndex, sort)

sample = [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
  ]

type Note = ([String], [String])

split :: Eq a => a -> [a] -> [[a]]
split = split' []
  where
    split' :: Eq a => [a] -> a -> [a] -> [[a]]
    split' acc _ [] = [reverse acc]
    split' acc s (a:as) =
      if s == a
      then (reverse acc) : (split' [] s as)
      else split' (a:acc) s as

parseNote :: String -> Note
parseNote i = (patterns, outputs)
  where
    [pattS, outS] = split '|' i
    patterns = split ' ' $ init pattS
    outputs = split ' ' $ tail outS

isSimple :: String -> Bool
isSimple = (flip elem [2, 4, 3, 7]) . length

get1478 :: [String] -> (String, String, String, String)
get1478 i = (findLen 2, findLen 4, findLen 3, findLen 7)
  where
    findLen :: Int -> String
    findLen l = head $ filter ((==l) . length) i

getDigits :: [String] -> [String]
getDigits i
  = map sort [zero, one, two, three, four, five, six, seven, eight, nine]
  where
    (one, four, seven, eight) = get1478 i
    [zero] = flip filter i $ commonL147 6 2 3 3
    two = eight `excluding` [b, f]
    [three] = flip filter i $ commonL147 5 2 3 3
    five = eight `excluding` [c, e]
    [six] = flip filter i $ commonL147 6 1 3 2
    [nine] = flip filter i $ commonL147 6 2 4 3

    [b] = five `excluding` three
    [c] = eight `excluding` six
    [e] = eight `excluding` nine
    [f] = one `intersection` five

    commonL147 :: Int -> Int -> Int -> Int -> String -> Bool
    commonL147 len i iv vii w
      =   length w == len
      &&  length (intersection w one) == i
      &&  length (intersection w four) == iv
      &&  length (intersection w seven) == vii

excluding :: Eq a => [a] -> [a] -> [a]
excluding a b = filter (not . flip elem b) a

intersection :: Eq a => [a] -> [a] -> [a]
intersection a b = filter (flip elem b) a

decodeDigit :: [String] -> String -> Int
decodeDigit code s = let (Just i) = findIndex (==s) code in i

decodeNote :: Note -> Int
decodeNote (pattern, output) = asInt $ map (decodeDigit code . sort) output
  where
    code = getDigits pattern
    asInt :: [Int] -> Int
    asInt = read . concatMap show

main = do
  f <- readFile "day08.input.txt"
  let notes = map parseNote $ lines f
  putStr "Part 1: "
  print $ length $ filter isSimple $ concatMap snd notes
  putStr "Part 2: "
  print $ sum $ map decodeNote notes