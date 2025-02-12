module Main (main) where

import Data.Map qualified as Map
import Debug.Trace (traceShow)

newtype Computer = Computer String
  deriving (Eq, Ord)

instance Show Computer where
  show (Computer s) = s

data Connection = Connection Computer Computer
  deriving (Ord)

instance Show Connection where
  show (Connection c1 c2) = show c1 ++ "-" ++ show c2

instance Eq Connection where
  (==) (Connection (Computer a1) (Computer b1)) (Connection (Computer a2) (Computer b2)) =
    (a1 == a2 && b1 == b2)
      || (a1 == b2 && a2 == b1)

newtype LAN = LAN (Map.Map Computer [Connection])
  deriving (Show)

addConnection :: LAN -> Connection -> LAN
addConnection (LAN m) conn =
  let (Connection c1 c2) = conn
      m' = Map.alter (Just . maybe [conn] (conn :)) c2 m
      m'' = Map.alter (Just . maybe [conn] (conn :)) c1 m'
   in LAN m''

part1 input =
  let m =
        foldr
          ( \connection m ->
              let (Connection c1 c2) = connection
                  m' = Map.alter (Just . maybe [connection] (connection :)) c2 m
                  m'' = Map.alter (Just . maybe [connection] (connection :)) c1 m'
               in m''
          )
          (Map.empty :: Map.Map Computer [Connection])
          input
   in LAN m

part2 input = input

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: ?
  print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?

processInput :: [Char] -> [Connection]
processInput contents =
  let lns = lines contents
      connections = map (\(ch1 : ch2 : '-' : ch3 : ch4 : _) -> Connection (Computer [ch1, ch2]) (Computer [ch3, ch4])) lns
   in connections
