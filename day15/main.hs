import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug.Trace (traceShow, traceShowId)
import Part1 (part1)
import Part2 (part2)
import Utils

main = do
  testFile <- readFile "./test.txt"
  inputFile <- readFile "./input.txt"

  putStrLn "\n----- Part 1 -----"
  -- print (part1 testFile) -- Expected: 10092
  -- print (part1 inputFile) -- Expected: 1437174
  putStrLn "\n----- Part 2 -----"
  print (part2 testFile) -- Expected: 9021
  print (part2 inputFile) -- Expected: 1437468
