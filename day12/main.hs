import Data.Maybe (catMaybes, fromJust)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug.Trace (traceShow, traceShowId)
import Grid qualified

type Plot = Grid.GridItem Char

type Garden = Grid.Grid Char

type Region = [Plot]

area :: Region -> Int
area = length

perimeter :: Region -> Int
perimeter region =
  let set = Set.fromList region
      plotPerimeter plot =
        let surrounding = Grid.surrounding plot
            edges = 4 - length surrounding
         in edges + length (filter (\p -> not (p `Set.member` set)) surrounding)
   in sum (map plotPerimeter region)

showRegion :: Garden -> Region -> String
showRegion garden plots =
  let plotsSet = Set.fromList plots
      showPlot plot = if plot `Set.member` plotsSet then Grid.value plot else '.'
      showRow row = Vector.toList (Vector.map showPlot row) ++ "\n"
   in concatMap showRow garden

part1 garden =
  let regions = getRegions garden
      areas = map area regions
      perimeters = map perimeter regions
      products = zipWith (*) areas perimeters
   in sum products

part2 input = ""

getRegions :: Garden -> [Region]
getRegions garden = fst (getRegions' garden Set.empty)
  where
    getRegions' :: Garden -> Set.Set Plot -> ([Region], Set.Set Plot)
    getRegions' garden visited =
      Vector.foldr
        ( flip
            ( Vector.foldr
                ( \curr (r, v) ->
                    if curr `Set.member` v
                      then (r, v)
                      else
                        let (r', v') = getRegion garden curr
                         in (r' : r, v `Set.union` v')
                )
            )
        )
        ([], visited)
        garden

getRegion :: Garden -> Plot -> (Region, Set.Set Plot)
getRegion garden plot = getRegion' garden plot plot Set.empty
  where
    getRegion' :: Garden -> Plot -> Plot -> Set.Set Plot -> (Region, Set.Set Plot)
    getRegion' garden start curr visited
      | curr `Set.member` visited = error ("Already visited: " ++ show curr)
      | Grid.value start /= Grid.value curr = ([], visited)
      | otherwise =
          foldr
            ( \curr (r, v) ->
                if curr `Set.member` v
                  then (r, v)
                  else
                    let (r', v') = getRegion' garden start curr v
                     in (r' ++ r, v')
            )
            ([curr], curr `Set.insert` visited)
            (Grid.surrounding curr)

processInput :: String -> Garden
processInput contents =
  let garden = Grid.fromList (lines contents)
      first = fromJust (Grid.getPosition garden (0, 4))
   in garden

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
