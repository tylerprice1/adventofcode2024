import Control.Exception (assert)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, maybe)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
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

straightLinePerimeter :: Garden -> Region -> Int
straightLinePerimeter garden region = length (regionEdges region)
  where
    regionEdges :: Region -> [Edge]
    regionEdges region = (Set.toList . Set.fromList) (foldr (\p edges -> plotEdges p ++ edges) [] region)
      where
        plotEdges :: Plot -> [Edge]
        plotEdges plot =
          if isInternal plot
            then []
            else
              let sameAsPlot = Grid.sameValue plot
                  top = followHorizontalEdge T plot
                  bottom = followHorizontalEdge B plot
                  left = followVerticalEdge L plot
                  right = followVerticalEdge R plot
               in catMaybes
                    [ maybe (Just (Edge L left)) (\l -> if sameAsPlot l then Nothing else Just (Edge L left)) (Grid.left plot),
                      maybe (Just (Edge T top)) (\u -> if sameAsPlot u then Nothing else Just (Edge T top)) (Grid.up plot),
                      maybe (Just (Edge R right)) (\r -> if sameAsPlot r then Nothing else Just (Edge R right)) (Grid.right plot),
                      maybe (Just (Edge B bottom)) (\d -> if sameAsPlot d then Nothing else Just (Edge B bottom)) (Grid.down plot)
                    ]
          where
            neighborGetters = [Grid.up, Grid.left, Grid.down, Grid.right]
            isInternal plot = all (maybe False (\i -> Grid.value i == Grid.value plot) . (\get -> get plot)) neighborGetters

            followWhile :: Plot -> (Plot -> Maybe Plot) -> (Plot -> Bool) -> [Plot]
            followWhile plot get keep = maybe [] (\next -> if keep next then next : followWhile next get keep else []) (get plot)

            followHorizontalEdge side plot = reverse (f plot Grid.left) ++ [plot] ++ f plot Grid.right
              where
                f plot get =
                  followWhile
                    plot
                    get
                    ( \p ->
                        Grid.sameValue plot p
                          && ( if side == T
                                 then maybe True (\u -> not (Grid.sameValue u p)) (Grid.up p)
                                 else maybe True (\d -> not (Grid.sameValue d p)) (Grid.down p)
                             )
                    )
            followVerticalEdge side plot = reverse (f plot Grid.up) ++ [plot] ++ f plot Grid.down
              where
                f plot get =
                  followWhile
                    plot
                    get
                    ( \p ->
                        Grid.sameValue plot p
                          && ( if side == L
                                 then maybe True (\l -> not (Grid.sameValue l p)) (Grid.left p)
                                 else maybe True (\r -> not (Grid.sameValue r p)) (Grid.right p)
                             )
                    )

showRegion :: Garden -> Region -> String
showRegion garden plots =
  let plotsSet = Set.fromList plots
      showPlot plot = if plot `Set.member` plotsSet then Grid.value plot else '.'
      showRow row = Vector.toList (Vector.map showPlot row) ++ "\n"
   in concatMap showRow garden

part1 (garden, regions) =
  let areas = map area regions
      perimeters = map perimeter regions
      products = zipWith (*) areas perimeters
   in sum products

part2 (garden, regions) =
  let areas = map area regions
      perimeters = map (straightLinePerimeter garden) regions
      products = zipWith (*) areas perimeters
   in sum products

processInput :: String -> (Garden, [Region])
processInput contents =
  let garden = Grid.fromList (lines contents)
      first = fromJust (Grid.getPosition garden (0, 4))
      regions = getRegions garden
   in (garden, regions)
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

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: 1930
  print (part1 input) -- Expected: 1387004
  putStrLn "\n----- Part 2 -----"
  print (part2 test) -- Expected: 1206
  print (part2 input) -- Expected: 844198
