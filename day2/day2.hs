import Data.List qualified
import Data.Map qualified
import Data.Maybe qualified
import Data.String qualified
import Debug.Trace qualified

readLine line = [read a :: Int | a <- words line]

readInput contents = [readLine line | line <- lines contents]

areSubsequent :: [Int] -> (Int -> Int -> Bool) -> Bool
areSubsequent [] _ = True
areSubsequent [value] _ = True
areSubsequent (first : second : rest) cmp = cmp first second && areSubsequent (second : rest) cmp

isDecreasing :: [Int] -> Bool
isDecreasing values = areSubsequent values (<)

isIncreasing :: [Int] -> Bool
isIncreasing values = areSubsequent values (>)

isPairSafe :: Int -> Int -> Bool
isPairSafe a b = diff >= 1 && diff <= 3
  where
    diff = abs (a - b)

isSafe l = (isIncreasing l || isDecreasing l) && areSubsequent l isPairSafe

part1 :: [[Int]] -> [[Int]] -> IO ()
part1 test input = do
  let safeTestLines = Data.List.filter isSafe test
  let safeInputLines = Data.List.filter isSafe input

  putStrLn ("Test:  " ++ show (Data.List.length safeTestLines)) -- 2 expected
  putStrLn ("Input: " ++ show (Data.List.length safeInputLines)) -- 252 expected

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (x : xs) = xs
remove n (x : xs) = x : remove (n - 1) (xs)

isCombinationSafe :: [Int] -> Bool
isCombinationSafe l = any isSafe combinations
  where
    combinations = l : [remove i l | i <- [0 .. (length l)]]

part2 test input = do
  putStrLn ("Test:  " ++ show (Data.List.length (filter id [isCombinationSafe l | l <- test]))) -- 4 expected
  putStrLn ("Input: " ++ show (Data.List.length (filter id [isCombinationSafe l | l <- input]))) -- 324 expected

main = do
  testFile <- readFile "./test.txt"
  let test = readInput testFile

  inputFile <- readFile "./input.txt"
  let input = readInput inputFile

  putStrLn "----- Part 1 -----"
  part1 test input

  putStrLn "\n----- Part 2 -----"
  part2 test input
