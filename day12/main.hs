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

edges :: Region -> [[Plot]]
edges region =
  fst (edges' edgePlots Set.empty)
  where
    edges' :: Region -> Set.Set Plot -> ([[Plot]], Set.Set Plot)
    edges' region visited =
      foldr
        ( \p (edges, v) ->
            if p `Set.member` v
              then (edges, v)
              else
                let (pEdges, v') = followEdge p v
                 in (pEdges ++ edges, v')
        )
        ([], visited)
        region

    edgePlots = filter (not . isInternal) region

    neighborGetters = [Grid.up, Grid.left, Grid.down, Grid.right]
    isInternal plot = all (maybe False (\i -> Grid.value i == Grid.value plot) . (\get -> get plot)) neighborGetters

    followEdge plot visited =
      foldr
        ( \get (edges, v) ->
            let (plots, v') = follow plot get v
             in (plots : edges, v')
        )
        ([], visited)
        neighborGetters
      where
        follow :: Plot -> (Plot -> Maybe Plot) -> Set.Set Plot -> ([Plot], Set.Set Plot)
        follow plot get visited =
          let maybeNext = get plot
           in case maybeNext of
                Nothing -> ([], visited)
                Just next ->
                  let (plots, visited') = follow next get visited
                   in (next : plots, visited')

straightLinePerimeter :: Region -> Int
straightLinePerimeter region = length (edges region)

showRegion :: Garden -> Region -> String
showRegion garden plots =
  let plotsSet = Set.fromList plots
      showPlot plot = if plot `Set.member` plotsSet then Grid.value plot else '.'
      showRow row = Vector.toList (Vector.map showPlot row) ++ "\n"
   in concatMap showRow garden

part1 regions =
  let areas = map area regions
      perimeters = map perimeter regions
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
