import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)

unique :: (Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

type Rise = Int

type Run = Int

type Slope = (Rise, Run)

type Position = (Int, Int)

getPairs :: [Position] -> [(Position, Position)]
getPairs [] = []
getPairs [only] = []
getPairs [a, b] = [(a, b)]
getPairs (first : rest) = map (first,) rest ++ getPairs rest

getSlope :: Position -> Position -> Slope
getSlope (y1, x1) (y2, x2) = ((y2 - y1) `div` denominator, (x2 - x1) `div` denominator)
  where
    rise = y2 - y1
    run = x2 - x1
    denominator = gcd rise run

processFile :: String -> (Int, Int, Map.Map Char [Position])
processFile contents = (height, width, map)
  where
    enumerate l = zip l [1 ..]
    lns = filter (/= "") (lines contents)
    height = length lns
    width = (length . head) lns
    positions =
      concat [[(char, (l, c)) | (char, c) <- enumerate line] | (line, l) <- enumerate lns]
    antennas = filter (\(ch, _) -> ch /= '.') positions
    map = foldr reducer Map.empty antennas
      where
        reducer (char, position) map = Map.insert char (position : existing) map
          where
            existing = fromMaybe [] (Map.lookup char map)

part1 :: (Int, Int, Map.Map Char [Position]) -> Int
part1 (height, width, map) =
  let entries = Map.toList map
      antinodePairs = concat [[getAntinodes pair | pair <- getPairs positions] | (_, positions) <- entries]
      antinodes = concat [[a, b] | (a, b) <- antinodePairs]
      validAntinodes = filter (\(y, x) -> y >= 1 && y <= height && x >= 1 && x <= width) antinodes
      validUniqueAntinodes = unique validAntinodes
   in length validUniqueAntinodes
  where
    getAntinodes (a, b) =
      let (rise, run) = getSlope a b
          (y1, x1) = a
          (y2, x2) = b
          first = (y1 - rise, x1 - run)
          second = (y2 + rise, x2 + run)
       in (first, second)

part2 :: (Int, Int, Map.Map Char [Position]) -> Int
part2 (height, width, antennasMap) =
  let entries = Map.toList antennasMap
      pairs :: [(Position, Position)]
      pairs = concatMap (getPairs . snd) entries

      antinodes :: [Position]
      antinodes = concat [concat (pairToList (getAntinodes pair)) | pair <- pairs]
      validAntinodes = filter contains antinodes
      validUniqueAntinodes = unique validAntinodes
   in length validUniqueAntinodes
  where
    pairToList :: (a, a) -> [a]
    pairToList (x, y) = [x, y]

    contains (y, x) = y >= 1 && y <= height && x >= 1 && x <= width

    getAntinodes :: (Position, Position) -> ([Position], [Position])
    getAntinodes (a, b) =
      let (rise, run) = getSlope a b
          (y1, x1) = a
          first = (y1 - rise, x1 - run)
          (preceding, _) = getAntinodes (first, a)
          allPreceding = a : first : preceding

          (y2, x2) = b
          second = (y2 + rise, x2 + run)
          (_, following) = getAntinodes (b, second)
          allFollowing = b : second : following
       in (if contains first then allPreceding else [a], if contains second then allFollowing else [b])

main = do
  testFile <- readFile "./test.txt"
  let test = processFile testFile

  inputFile <- readFile "./input.txt"
  let input = processFile inputFile

  putStrLn "----- Part 1 -----"
  print (part1 test) -- expected: 14
  print (part1 input) -- expected: 280
  putStrLn "----- Part 2 -----"
  print (part2 test) -- expected: 34
  print (part2 input) -- expected: 958
