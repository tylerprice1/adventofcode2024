import Data.Maybe (catMaybes, fromJust, fromMaybe, maybe)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug.Trace (trace, traceShow, traceShowId)
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

straightLinePerimeter :: Region -> Int
straightLinePerimeter region =
  let (sum, _) = foldr foldFn (0, Set.empty) edgePlots
   in sum
  where
    foldFn plot (sum, visited) =
      let (sum', v') = sidePerimeter plot visited
       in (sum + sum', v')

    edgePlots = filter (not . isInternal) region
    set = Set.fromList edgePlots

    nextFns = [Grid.up, Grid.left, Grid.down, Grid.right]

    isInternal plot =
      maybe False (\i -> Grid.value i == Grid.value plot) (Grid.up plot)
        && maybe False (\i -> Grid.value i == Grid.value plot) (Grid.left plot)
        && maybe False (\i -> Grid.value i == Grid.value plot) (Grid.down plot)
        && maybe False (\i -> Grid.value i == Grid.value plot) (Grid.right plot)

    sidePerimeter :: Plot -> Set.Set Plot -> (Int, Set.Set Plot)
    sidePerimeter plot visited =
      let (sum, _, v') = foldr fn (0, plot, visited) nextFns
       in (sum, v')
      where
        fn :: (Grid.GridItem Char -> Maybe (Grid.GridItem Char)) -> (Int, Plot, Set.Set Plot) -> (Int, Plot, Set.Set Plot)
        fn getNext (sum, p, v) =
          let (p', v') = findStraightLineEndpoint p getNext v
           in (1 + sum, p', v')

    findStraightLineEndpoint plot getNext visited
      | plot `Set.member` visited = error "Already visited"
      | otherwise =
          let updatedVisited = plot `Set.insert` visited
           in case traceShow (plot, getNext plot) getNext plot of
                Nothing -> (plot, visited)
                Just p ->
                  if p `Set.member` set && not (p `Set.member` visited) && not (isInternal plot)
                    then traceShow ("Curr", plot, "Next", p) findStraightLineEndpoint p getNext updatedVisited
                    else (plot, visited)

showRegion :: Garden -> Region -> String
showRegion garden plots =
  let plotsSet = Set.fromList plots
      showPlot plot = if plot `Set.member` plotsSet then Grid.value plot else '.'
      showRow row = Vector.toList (Vector.map showPlot row) ++ "\n"
   in concatMap showRow garden

part1 regions =
  let areas = map area regions
      perimeters = (map perimeter regions)
      products = zipWith (*) areas perimeters
   in sum products

part2 regions =
  let areas = map area regions
      perimeters = map straightLinePerimeter regions
      products = zipWith (*) areas (trace ("\nPerimeters:" ++ foldr (\r acc -> show r ++ "\n" ++ acc) "\n" (zip perimeters regions)) perimeters)
   in sum products

processInput :: String -> [Region]
processInput contents =
  let garden = Grid.fromList (lines contents)
      first = fromJust (Grid.getPosition garden (0, 4))
      regions = getRegions garden
   in regions
  where
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

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  -- putStrLn "\n----- Part 1 -----"
  -- print (part1 test) -- Expected: ?
  -- print (part1 input) -- Expected: ?
  putStrLn "\n----- Part 2 -----"
  print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?
