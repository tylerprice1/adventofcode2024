import Control.Exception (assert)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, maybe)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.IO.Exception (assertError)
import Grid qualified

type Plot = Grid.GridItem Char

type Garden = Grid.Grid Char

type Region = [Plot]

area :: Region -> Int
area = length

plotPerimeter :: (Eq a) => Grid.GridItem a -> Int
plotPerimeter plot = length (filter (maybe True (\p -> Grid.value p /= Grid.value plot)) (Grid.maybeSurrounding plot))

showPrettyList :: (Show a) => [a] -> String
showPrettyList [] = "[]"
showPrettyList l = "[\n" ++ foldr (\el str -> "\t" ++ show el ++ "\n" ++ str) "" l ++ "]"

perimeter :: Region -> Int
perimeter region =
  let set = Set.fromList region
   in sum (map plotPerimeter region)

data Side = L | T | R | B
  deriving (Eq, Show, Ord)

data Edge = Edge Side [Plot]
  deriving (Eq, Show, Ord)

edges :: Region -> [Edge]
edges region =
  (Set.toList . Set.fromList)
    ( foldr
        ( \p edges ->
            let pEdges = followEdge p
             in {- trace ("\nPlot:  " ++ show p ++ "\nEdges: " ++ showPrettyList pEdges) -} (pEdges ++ edges)
        )
        []
        region
    )
  where
    edgePlots = filter (not . isInternal) region

    neighborGetters = [Grid.up, Grid.left, Grid.down, Grid.right]
    isInternal plot = all (maybe False (\i -> Grid.value i == Grid.value plot) . (\get -> get plot)) neighborGetters

    follow :: Plot -> (Plot -> Maybe Plot) -> [Plot]
    follow plot get = case get plot of
      Nothing -> []
      Just next
        | Grid.value next == Grid.value plot -> next : follow next get
        | otherwise -> []

    followHorizontalEdge plot = reverse (follow plot Grid.left) ++ [plot] ++ follow plot Grid.right
    followVerticalEdge plot = reverse (follow plot Grid.up) ++ [plot] ++ follow plot Grid.down

    followEdge :: Plot -> [Edge]
    followEdge plot =
      if isInternal plot
        then []
        else
          let l = Grid.left plot
              r = Grid.right plot
              u = Grid.up plot
              d = Grid.down plot

              horizontal = followHorizontalEdge plot
              vertical = followVerticalEdge plot
           in catMaybes
                [ maybe (Just (Edge L vertical)) (\p -> if Grid.sameValue plot p then Nothing else Just (Edge L vertical)) l,
                  maybe (Just (Edge T horizontal)) (\p -> if Grid.sameValue plot p then Nothing else Just (Edge T horizontal)) u,
                  maybe (Just (Edge R vertical)) (\p -> if Grid.sameValue plot p then Nothing else Just (Edge R vertical)) l,
                  maybe (Just (Edge B horizontal)) (\p -> if Grid.sameValue plot p then Nothing else Just (Edge B horizontal)) u
                ]

straightLinePerimeter :: Region -> Int
straightLinePerimeter region =
  length
    ( trace
        ( "\nRegion: "
            ++ show region
            ++ foldr
              (\p acc -> "\nEdges:  " ++ show p ++ acc)
              ""
              (edges region)
        )
        (edges region)
    )

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
      products = zipWith (*) areas (trace (foldr (\(count, r) acc -> show count ++ "\t" ++ show r ++ "\n" ++ acc) "\n" (zip perimeters regions)) perimeters)
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
