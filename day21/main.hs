import Data.Bifunctor qualified
import Data.List (find)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Dijkstra (Graph (..), Node (..), dijkstra, findNode)

navigateNumericKeypad :: Graph Char Int -> Node Char Int -> Node Char Int -> (Int, [Node Char Int])
navigateNumericKeypad = dijkstra

navigateDirectionalKeypad :: Graph Char Int -> Node Char Int -> Node Char Int -> (Int, [Node Char Int])
navigateDirectionalKeypad = dijkstra

pressKey :: Graph Char Int -> Node Char Int -> Node Char Int -> ([Node Char Int], [Node Char Int])
pressKey graph start key =
  let (_, fromStartToKey) = dijkstra graph start key
      (_, fromKeyToA) = dijkstra graph key (fromJust (findNode graph 'A'))
   in {- trace
        ("\n" ++ "Press key " ++ show key ++ " starting at " ++ show start ++ "\n") -}
      (fromStartToKey, tail fromKeyToA)

navigateSequence :: Graph Char Int -> String -> [[Node Char Int]]
navigateSequence _ "" = error "Empty"
navigateSequence _ [ch] = error "Need two"
navigateSequence graph (startCh : endCh : s) =
  let start = fromJust (findNode graph startCh)
      end = fromJust (findNode graph endCh)
      (_, path) = dijkstra graph start end
      rest = navigateSequence graph (endCh : s)
   in case s of
        "" -> [path]
        _ -> path : (tail . head) rest : tail rest

part1 :: ([String], Graph Char Int, Graph Char Int) -> [[[Node Char Int]]]
part1 (sequences, numericKeypad, directionalKeypad) = map (\s -> navigateSequence numericKeypad ('A' : s)) sequences

-- map
--   (\s -> foldr id s)
--   []
--   sequences

part2 input = input

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  mapM_ print (zip (let (t, _, _) = test in t) (part1 test)) -- Expected: ?
  -- print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?

processInput :: String -> ([String], Graph Char Int, Graph Char Int)
processInput contents = (lines contents, numericKeypad, directionalKeypad)
  where
    edge node = (1, node)

    -- - +---+---+---+
    -- - | 7 | 8 | 9 |
    -- - +---+---+---+
    -- - | 4 | 5 | 6 |
    -- - +---+---+---+
    -- - | 1 | 2 | 3 |
    -- - +---+---+---+
    -- -     | 0 | A |
    -- -     +---+---+
    numericKeypad :: Graph Char Int
    numericKeypad = Graph [nA, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9]
      where
        nA = Node 'A' [edge n0, edge n3]
        n0 = Node '0' [edge nA, edge n2]
        n1 = Node '1' [edge n2, edge n4]
        n2 = Node '2' [edge n0, edge n1, edge n5, edge n3]
        n3 = Node '3' [edge nA, edge n2, edge n6]
        n4 = Node '4' [edge n1, edge n5, edge n7]
        n5 = Node '5' [edge n2, edge n4, edge n6, edge n8]
        n6 = Node '6' [edge n3, edge n5, edge n9]
        n7 = Node '7' [edge n4, edge n8]
        n8 = Node '8' [edge n5, edge n7, edge n9]
        n9 = Node '9' [edge n6, edge n8]

    -- -     +---+---+
    -- -     | ^ | A |
    -- - +---+---+---+
    -- - | < | v | > |
    -- - +---+---+---+
    directionalKeypad :: Graph Char Int
    directionalKeypad = Graph [nA, nU, nL, nR, nD]
      where
        nA = Node 'A' [edge nU, edge nR]
        nU = Node '^' [edge nA, edge nD]
        nL = Node '<' [edge nD]
        nR = Node '>' [edge nA, edge nD]
        nD = Node 'v' [edge nL, edge nU, edge nR]
