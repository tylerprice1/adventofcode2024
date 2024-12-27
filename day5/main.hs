import Control.Exception (throw, throwIO)
import Data.Char
import Data.List qualified as List
import Data.Map qualified as Map
import Debug.Trace

type Page = Int

type Rule = (Page, Page)

type RulesMap = Map.Map Page [Page]

type Update = [Page]

areListsEqual :: (Eq a) => [a] -> [a] -> Bool
areListsEqual [] [] = True
areListsEqual [a] [] = False
areListsEqual [] [b] = False
areListsEqual (a : as) (b : bs) = a == b && areListsEqual as bs

split :: String -> Char -> [String]
split "" ch = []
split str ch =
  foldr
    ( \curr (first : rest) ->
        if curr == ch
          then "" : first : rest
          else (curr : first) : rest
    )
    [""]
    str

processFile :: String -> (RulesMap, [Update])
processFile contents =
  let ls = lines contents
      (ruleLines, updateLines) = List.partition ('|' `elem`) (filter (/= "") ls)
      rules =
        [ let [a, b] = split l '|'
           in (read a :: Int, read b :: Int)
          | l <- ruleLines
        ]
      rulesMap = foldr reducer (Map.empty :: RulesMap) rules
        where
          reducer :: Rule -> RulesMap -> RulesMap
          reducer (left, right) map = case Map.lookup left map of
            Nothing -> Map.insert left [right] map
            Just rules -> Map.insert left (right : rules) map

      updates = [[read n :: Int | n <- split l ','] | l <- updateLines]
   in (rulesMap, updates)

part1 input =
  let (rulesMap, updates) = processFile input
      compare a b = case Map.lookup a rulesMap of
        Nothing -> case compare b a of
          LT -> GT
          EQ -> EQ
          GT -> LT
        Just pages -> if b `elem` pages then LT else GT
      correct = filter (\update -> areListsEqual (List.sortBy compare update) update) updates
      middlePages = List.map (\pages -> pages !! div (length pages) 2) correct
   in List.sum middlePages

part2 input =
  let (rulesMap, updates) = processFile input
      compare a b = case Map.lookup a rulesMap of
        Nothing -> case compare b a of
          LT -> GT
          EQ -> EQ
          GT -> LT
        Just pages -> if b `elem` pages then LT else GT
      incorrect = filter (\update -> not (areListsEqual (List.sortBy compare update) update)) updates
      corrected = [List.sortBy compare i | i <- incorrect]
      middlePages = List.map (\pages -> pages !! div (length pages) 2) corrected
   in List.sum middlePages

main = do
  test <- readFile "./test.txt"
  input <- readFile "./input.txt"

  putStrLn "----- Part 1 -----"
  print (part1 test) -- expected 143
  print (part1 input) -- expected 4959
  putStrLn "----- Part 2 -----"
  print (part2 test) -- expected 123
  print (part2 input) -- expected 4655
