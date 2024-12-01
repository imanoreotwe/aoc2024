import System.IO
import Data.Maybe (mapMaybe) 

main = do
    contents <- readFile "input_1.txt"
    let splits = map words (lines $ contents) :: [[String]]
    let (firstList, secondList) = buildLists splits
    let total = calc firstList secondList
    print total 

buildLists :: [[String]] -> ([Int], [Int])
buildLists lists = (firstElems, secondElems)
    where
        intLists = [map readInt sublist | sublist <- lists]
        firstElems = sort [head sublist | sublist <- intLists, length sublist > 0]
        secondElems = sort [sublist !! 1 | sublist <- intLists , length sublist > 1]

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

calc [] []                  = 0
calc xs@(x:xt) ys@(y:yt)    = abs (x-y) + calc xt yt