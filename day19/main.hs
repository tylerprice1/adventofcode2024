{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use zipWith" #-}

-- import Control.Parallel (par, pseq)
-- import Control.Parallel.Strategies (dot, parList, parMap, rdeepseq, rpar, rparWith)
import Data.Bifunctor (Bifunctor (first))
import Data.Either (fromRight)
import Data.List (find, sort, sortOn)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Debug.Trace (trace, traceShowId)
import Text.Parsec (char, choice, letter, many, many1, newline, optional, parse, runParser, string)
import Text.Parsec.String (Parser)

-- trace a b = b

traceWithId :: (Show a) => String -> a -> a
traceWithId a b = trace (a ++ " " ++ show b) b

traceWithFn :: (Show a) => (a -> String) -> a -> a
traceWithFn fn a = trace (fn a) a

data Tree
  = Node Char [Tree]
  | Leaf Char
  | Root [Tree]
  deriving (Eq)

instance Ord Tree where
  compare (Leaf a) (Leaf b) = compare a b
  compare (Leaf _) _ = GT
  compare (Root _) _ = GT
  compare _ (Leaf _) = LT
  compare _ (Root _) = LT
  compare (Node ch1 _) (Node ch2 _) = compare ch1 ch2

instance Show Tree where
  show tree = showTree tree 0

breakOn :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
breakOn predicate list =
  let (initial, rest) = break predicate list
   in case rest of
        [] -> Nothing
        (el : post) -> Just (initial, el, post)

insert :: String -> Tree -> Tree
insert s (Leaf _) = error "Cannot insert into leaf"
insert s (Root nodes) = Root (insertNodes s nodes)
insert s (Node nch nodes) = Node nch (insertNodes s nodes)

nodeHasChar :: Tree -> Char -> Bool
nodeHasChar (Root _) _ = False
nodeHasChar (Leaf lch) ch = ch == lch
nodeHasChar (Node nch _) ch = ch == nch

insertNodes :: String -> [Tree] -> [Tree]
insertNodes "" nodes = nodes
insertNodes [ch] nodes =
  let leaf = Leaf ch
   in if leaf `elem` nodes
        then error ("duplicate " ++ show ch)
        else leaf : nodes
insertNodes (ch : s) nodes = case breakOn (`nodeHasChar` ch) nodes of
  Nothing -> insertNodes (ch : s) (Node ch [] : nodes)
  Just (leading, node, trailing) -> case node of
    Node nch nodes' -> leading ++ insert s (Node nch nodes') : trailing
    Leaf lch -> leading ++ Leaf lch : insert (s) (Node ch []) : trailing

showTrees :: [Tree] -> Int -> String
showTrees nodes depth = foldr (\n s -> showTree n depth ++ s) "" nodes

showTree :: Tree -> Int -> String
showTree (Leaf ch) depth = replicate depth '\t' ++ "- " ++ [ch] ++ "\n"
showTree (Node ch nodes) depth = replicate depth '\t' ++ "| " ++ show ch ++ "\n" ++ showTrees nodes (depth + 1)
showTree (Root nodes) depth = replicate depth '\t' ++ "| \n" ++ showTrees nodes (depth + 1)

treeToList :: Tree -> [String]
treeToList (Leaf ch) = [[ch]]
treeToList (Root nodes) = concatMap treeToList nodes
treeToList (Node ch nodes) = map (ch :) (concatMap treeToList nodes)

countCombinations :: String -> Tree -> Map.Map String Int -> (Int, Map.Map String Int)
countCombinations "" tree cache = error "empty"
countCombinations s tree cache = case Map.lookup s cache of
  Just m -> (m, cache)
  Nothing ->
    case {- traceWithFn (\m -> s ++ "   " ++ show m) -} (findMatches s tree) of
      Nothing -> (0, Map.insert s 0 cache)
      Just matches ->
        let (count, cache') =
              foldr
                ( \(match, remaining) (count, cache') ->
                    if null remaining
                      then (count + 1, cache')
                      else
                        let (count', cache'') = countCombinations remaining tree cache'
                         in (count' + count, cache'')
                )
                (0, cache)
                matches
         in (count, Map.insert s count cache')
  where
    findMatches :: String -> Tree -> Maybe [(String, String)]
    findMatches "" tree = Nothing
    findMatches [ch] (Leaf lch) = if ch == lch then Just [([lch], "")] else Nothing
    findMatches (ch : s) (Leaf lch) = if ch == lch then Just [([lch], s)] else Nothing
    findMatches (ch : s) (Node nch nodes) =
      if ch == nch
        then case mapMaybe (findMatches s) nodes of
          [] -> Nothing
          matches -> Just (map (first (nch :)) (concat matches))
        else Nothing
    findMatches s (Root nodes) = case concat (mapMaybe (findMatches s) nodes) of
      [] -> Nothing
      matches -> Just matches

part1 :: ([String], [String]) -> Int
part1 (towels, designs) = length (filter (\d -> 0 /= fst (countCombinations d tree Map.empty)) designs)
  where
    tree :: Tree
    tree = foldr insert (Root []) towels

part2 :: ([String], [String]) -> Int
part2 (towels, designs) = sum counts
  where
    counts = map (\d -> traceWithFn (\c -> show c ++ "    " ++ d) (fst (countCombinations d tree Map.empty))) designs

    tree :: Tree
    tree = traceShowId (foldr insert (Root []) towels)

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  test2File <- readFile "./test2.txt"
  let test2 = processInput test2File

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  -- print (part1 test) -- Expected: 6
  -- print (part1 input) -- Expected: 258
  putStrLn "\n----- Part 2 -----"
  print (part2 test) -- Expected: 16
  print (part2 test2) -- Expected: 2
  print (part2 input) -- Expected: ?

processInput :: String -> ([String], [String])
processInput contents = fromRight ([], []) (parse parser "" contents)
  where
    parser :: Parser ([String], [String])
    parser = do
      towels <- towelsParser
      _ <- many1 newline
      designs <- designsParser
      return (towels, designs)

    towelsParser :: Parser [String]
    towelsParser = many (many1 letter <* optional (string ", "))

    designsParser :: Parser [String]
    designsParser = many (many1 letter <* optional newline)
