import Control.DeepSeq (deepseq)
import Data.Bits (Bits (xor))
import GHC.List (foldr')

mix :: Int -> Int -> Int
mix a b = a `xor` b

prune :: Int -> Int
prune a = a `mod` 16777216

evolve :: Int -> Int
evolve a =
  let -- Calculate the result of multiplying the secret number by 64. Then, mix this result into the secret number. Finally, prune the secret number.
      step1 = prune ((a * 64) `mix` a)
      -- Calculate the result of dividing the secret number by 32. Round the result down to the nearest integer. Then, mix this result into the secret number. Finally, prune the secret number.
      step2 = step1 `deepseq` prune ((step1 `div` 32) `mix` step1)
      -- Calculate the result of multiplying the secret number by 2048. Then, mix this result into the secret number. Finally, prune the secret number.
      step3 = step2 `deepseq` prune ((step2 * 2048) `mix` step2)
   in step3 `deepseq` step3

generate :: Int -> [Int]
generate start =
  let next = evolve start
   in next : generate next

onesPlace :: Int -> Int
onesPlace = (`mod` 10)

changes :: [Int] -> [Int]
changes [] = []
changes [_] = []
changes (a : b : rest) = (b - a) : changes (b : rest)

part1 input = sum (map ((!! 1999) . generate) input)

part2 input = map (take 10 . changes . generate) input

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: 37327623
  print (part1 input) -- Expected: 13429191512
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?

processInput :: String -> [Int]
processInput contents = map (\l -> read l :: Int) (lines contents)
