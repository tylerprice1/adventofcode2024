module Main (main) where

part1 input = input

part2 input = input

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: ?
  print (part1 input) -- Expected: ?
  putStrLn "\n----- Part 2 -----"
  print (part2 test) -- Expected: ?
  print (part2 input) -- Expected: ?

processInput contents = contents
