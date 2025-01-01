processInput contents = contents

part1 input = ""

part2 input = ""

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "----- Part 1 -----"
  print (part1 test) -- Expected: ?
  print (part1 input) -- Expected: ?
  putStrLn "----- Part 2 -----"
  print (part2 test) -- Expected: ?
  print (part2 input) -- Expected: ?
