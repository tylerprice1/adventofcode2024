import Control.Parallel qualified as Parallel
import Data.Map qualified as Map
import Debug.Trace (traceShow, traceShowId)

type Depth = Int

type Stone = Int

type UpdatedStone = Either Stone (Stone, Stone)

type StoneResultCache = Map.Map (Stone, Depth) Int

numDigits :: Int -> Int
numDigits n
  | n >= 1_000_000 = 6 + numDigits (n `div` 1_000_000)
  | n >= 100_000 = 5 + numDigits (n `div` 100_000)
  | n >= 10_000 = 4 + numDigits (n `div` 10_000)
  | n >= 1_000 = 3 + numDigits (n `div` 1_000)
  | n >= 100 = 2 + numDigits (n `div` 100)
  | n >= 10 = 1 + numDigits (n `div` 10)
  | otherwise = 1

strToStone :: String -> Stone
strToStone s = read s :: Stone

updateStone :: Stone -> UpdatedStone
updateStone 0 = Left 1
updateStone stone =
  let len = numDigits stone
   in if even len
        then
          let (l, r) = splitAt (len `div` 2) (show stone)
           in Right (strToStone l, strToStone r)
        else Left (2024 * stone)

updatedStoneToList (Left n) = [n]
updatedStoneToList (Right (a, b)) = [a, b]

updateStones :: [Stone] -> [Stone]
updateStones = foldr f []
  where
    f :: Stone -> [Stone] -> [Stone]
    f stone updatedStones = either (: updatedStones) (\(l, r) -> l : (r : updatedStones)) (updateStone stone)

updateN :: Int -> Stone -> Int
updateN n stone = result
  where
    (result, _) = updateN' 1 n [stone] Map.empty

    updateN' :: Int -> Int -> [Stone] -> StoneResultCache -> (Int, StoneResultCache)
    updateN' depth maxDepth stones cache =
      if depth <= maxDepth
        then
          let f :: Stone -> (Int, StoneResultCache) -> (Int, StoneResultCache)
              f stone (sum, cache) = case Map.lookup (stone, depth) cache of
                Just count -> (sum + count, cache)
                Nothing ->
                  let (result, updatedCache') = updateN' (depth + 1) maxDepth ((updatedStoneToList . updateStone) stone) cache
                      updatedCache = Map.insert (stone, depth) result updatedCache'
                   in (sum + result, updatedCache)
           in foldr f (0, cache) stones
        else
          let result = length stones
           in (result, Map.insert (stone, depth) result cache)

updateAllN :: Int -> [Stone] -> Int
updateAllN n stones = sum (map (updateN n) stones)

part1 :: [Stone] -> Int
part1 = updateAllN 25

part2 :: [Stone] -> Int
part2 = updateAllN 75

processInput :: String -> [Stone]
processInput contents = map strToStone (words contents)

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
