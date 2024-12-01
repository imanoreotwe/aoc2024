import System.IO
import Data.Map (Map)
import qualified Data.Map.Strict as Map

main = do
    contents <- readFile "input_1.txt"
    let splits = map words (lines $ contents) :: [[String]]
    let (firstList, secondList) = buildLists splits
    let similarityDict = buildDickt secondList Map.empty
    let total = calc firstList similarityDict
    print total 

buildLists :: [[String]] -> ([Int], [Int])
buildLists lists = (firstElems, secondElems)
    where
        intLists = [map readInt sublist | sublist <- lists]
        firstElems = [head sublist | sublist <- intLists, length sublist > 0]
        secondElems = [sublist !! 1 | sublist <- intLists , length sublist > 1]

readInt :: String -> Int
readInt s = read s

sort :: [Int] -> [Int]
sort []     = []
sort [x]    = [x]
sort list   = let (a,b) = split list
              in merge (sort a) (sort b)

merge [] ys                             = ys 
merge xs []                             = xs 
merge xs@(x:xt) ys@(y:yt) | x <= y      = x : merge xt ys
                          | otherwise   = y : merge xs yt

split :: [Int] -> ([Int], [Int])
split myList = splitAt (((length myList) + 1) `div` 2) myList

buildDickt :: (Ord k) => [k] -> Map.Map k Int -> Map.Map k Int
buildDickt [] dick      = dick
buildDickt (x:xs) dick  =
    case Map.lookup x dick of
        Nothing     -> buildDickt xs (Map.insert x 1 dick)
        Just count  -> buildDickt xs (Map.insert x (count + 1) dick)


calc [] _           = 0
calc (x:xt) dick    = 
    case Map.lookup x dick of
        Just val    -> x * val + calc xt dick
        Nothing     -> calc xt dick