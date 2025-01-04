import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug.Trace (traceShow, traceShowId)
import Grid qualified

type Plot = Grid.GridItem Char

type Garden = Grid.Grid Char

type Region = [Plot]

showRegion :: Garden -> Region -> String
showRegion garden plots =
  let plotsSet = Set.fromList plots
      showPlot plot = if plot `Set.member` plotsSet then Grid.value plot else '.'
      showRow row = Vector.toList (Vector.map showPlot row) ++ "\n"
   in concatMap showRow garden

part1 garden = map (showRegion garden) (getRegions garden)

part2 input = ""

getRegions :: Garden -> [Region]
getRegions garden = fst (getRegions' garden Set.empty)
  where
    getRegions' :: Garden -> Set.Set Plot -> ([Region], Set.Set Plot)
    getRegions' garden visited =
      Vector.foldr
        ( \row (r, v) ->
            Vector.foldr
              ( \curr acc ->
                  if curr `Set.member` v
                    then acc
                    else
                      let (r, v) = acc
                          (r', v') = getRegion garden curr
                       in (r' : r, v `Set.union` v')
              )
              (r, v)
              row
        )
        ([], visited)
        garden

getRegion :: Garden -> Plot -> (Region, Set.Set Plot)
getRegion garden plot = getRegion' garden plot plot Set.empty
  where
    getRegion' :: Garden -> Plot -> Plot -> Set.Set Plot -> (Region, Set.Set Plot)
    getRegion' garden start curr visited
      | curr `Set.member` visited = error ("Already visited: " ++ show curr ++ " " ++ show visited)
      | Grid.value start /= Grid.value curr = ([], curr `Set.insert` visited)
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
            (filter (\p -> not (p `Set.member` visited)) (Grid.surrounding curr))

processInput :: String -> Garden
processInput contents =
  let garden = Grid.fromList (lines contents)
      first = fromJust (Grid.getPosition garden (0, 4))
   in garden

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  -- inputFile <- readFile "./input.txt"
  -- let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  mapM_ putStrLn (part1 test) -- Expected: ?
  -- print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?
