import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map, (!), empty, findWithDefault, insertWith, keys)

sample :: String
sample = unlines [
    "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
  ]

type Path = [String]
type Fs = Map Path [Either String (String, Int)]

cd :: Path -> String -> Path
cd _ "/" = []
cd p ".." = tail p
cd p d = d : p

indexOf :: Eq a => a -> [a] -> Int
indexOf a' (a:as) = if a' == a then 0 else 1 + indexOf a' as

split :: Eq a => a -> [a] -> ([a], [a])
split c s = let i = indexOf c s in (take i s, drop (succ i) s)

file :: Fs -> Path -> String -> Int -> Fs
file fs p f s = insertWith (++) p [Right (f, s)] fs

dir :: Fs -> Path -> String -> Fs
dir fs p d = insertWith (++) p [Left d] fs

parse :: Path -> Fs -> [String] -> Fs
parse _ fs [] = fs
parse path fs (l:ls) = let (l1, l2) = split ' ' l in
  if "$ ls" == l                  then parse path fs ls
  else if "$ cd " `isPrefixOf` l  then parse (cd path $ drop 5 l) fs ls
  else if l1 == "dir"             then parse path (dir fs path l2) ls
                                  else parse path (file fs path l2 (read l1)) ls

size :: Path -> Fs -> Int
size p fs = sum $ flip map (findWithDefault [] p fs) $ \e -> case e of
  Left d -> size (d:p) fs
  Right (_, s) -> s

sizes :: Fs -> [Int]
sizes fs = map (flip size fs) (keys fs)

totalSize = 70000000
requiredSize = 30000000

main = do
  f <- readFile "day07.input.txt"
  let fs = parse [] empty $ filter (not.null) $ lines f
  let ss = sizes fs
  putStr "Part 1: "
  print $ sum $ filter (< 100000) ss 
  putStr "Part 2: "
  let available = totalSize - (size [] fs)
  let minDirSizeToDelete = requiredSize - available
  print $ head $ sort $ filter (>= minDirSizeToDelete) ss  
