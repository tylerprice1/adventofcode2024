import Control.Parallel qualified as Parallel
import Data.Map (Map, empty)
import Debug.Trace (traceShow, traceShowId)

type Stone = (Int, String)

type UpdatedStone = Either Stone (Stone, Stone)

type StoneUpdateCache = Map Stone UpdatedStone

numDigits :: Int -> Int
numDigits n
  | n >= 1_000_000 = 6 + numDigits (n `div` 1_000_000)
  | n >= 100_000 = 5 + numDigits (n `div` 100_000)
  | n >= 10_000 = 4 + numDigits (n `div` 10_000)
  | n >= 1_000 = 3 + numDigits (n `div` 1_000)
  | n >= 100 = 2 + numDigits (n `div` 100)
  | n >= 10 = 1 + numDigits (n `div` 10)
  | otherwise = 1

intToStone :: Int -> (Int, String)
intToStone n = (n, show n)

strToStone :: String -> (Int, String)
strToStone s = intToStone (read s :: Int)

processInput :: String -> [(Int, String)]
processInput contents = map strToStone (words contents)

updateStone :: Stone -> StoneUpdateCache -> UpdatedStone
updateStone (0, _) cache = Left (1, "1")
updateStone (n, s) cache =
  let len = numDigits n
   in if even len
        then
          let (l, r) = splitAt (len `div` 2) s
           in Right (strToStone l, strToStone r)
        else Left (intToStone (2024 * n))

updateStones :: [Stone] -> StoneUpdateCache -> [Stone]
updateStones stones cache = result
  where
    result = foldr f [] stones

    f :: Stone -> [Stone] -> [Stone]
    f stone updatedStones = either (: updatedStones) (\(l, r) -> l : (r : updatedStones)) (updateStone stone cache)

updateN :: Int -> Stone -> Int
updateN n stone = updateN' 0 n [stone] empty
  where
    updateN' depth maxDepth stones cache =
      if depth < maxDepth
        then sum (map (\s -> updateN' (depth + 1) maxDepth [s] cache) (updateStones stones cache))
        else length stones

updateAllN :: Int -> [Stone] -> Int
updateAllN n stones = sum (map (updateN n) stones)

part1 :: [Stone] -> Int
part1 stones = updateAllN 25 stones -- length (foldr (\_ stones -> updateAll stones) stones [1 .. 25])

part2 :: [Stone] -> Int
part2 stones = updateAllN 75 stones -- length (foldr (\_ stones -> updateAll stones) stones [1 .. 75])

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: 55312
  print (part1 input) -- Expected: 218956
  putStrLn "\n----- Part 2 -----"
  print (part2 test) -- Expected: ?
  print (part2 input) -- Expected: ?
