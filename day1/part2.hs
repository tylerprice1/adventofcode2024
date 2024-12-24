import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.String qualified as String
import System.IO qualified as IO

readLine :: String -> (Int, Int)
readLine line = (read a :: Int, read b :: Int)
  where
    (a : b : _) = String.words line

readInput :: String -> ([Int], [Int])
readInput contents = unzip [readLine line | line <- lines]
  where
    lines = String.lines contents

getMap :: [Int] -> Map.Map Int Int
getMap [] = Map.empty
getMap [value] = Map.fromList [(value, 1)]
getMap (value : rest) =
  case Map.lookup value map of
    Nothing -> Map.insert value 1 map
    Just count -> Map.insert value (count + 1) map
  where
    map = getMap rest

getCount value map =
  Maybe.fromMaybe 0 (Map.lookup value map)

main = do
  contents <- IO.readFile "./input.txt"
  let (a, b) = readInput contents

  let map = getMap b
  let counts = [getCount value map | value <- a]
  let scores = [value * count | (value, count) <- zip a counts]
  let score = sum scores

  return score
