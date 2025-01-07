import Control.Monad (void)
import Data.Char (isDigit)
import Data.List (groupBy, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Debug.Trace (trace, traceShow, traceShowId)
import Text.Parsec (char, digit, many1, optionMaybe, parse, string)
import Text.Parsec.String (Parser)

type X = Int

type Y = Int

data Coordinate = Coordinate {x :: X, y :: Y}
  deriving (Eq)

instance Ord Coordinate where
  compare a b = case compare (x a) (x b) of
    EQ -> compare (y a) (y b)
    comp -> comp

instance Show Coordinate where
  show c = "(" ++ show (x c) ++ ", " ++ show (y c) ++ ")"

type Position = Coordinate

type Velocity = Coordinate

data Robot = Robot {position :: Position, velocity :: Velocity}
  deriving (Show)

showSpace width height robots =
  let midY = (height `div` 2)
      midX = (width `div` 2)
      positionMap = Map.fromList (map (\r -> (position r, r)) robots)
   in [ [ if y == midY || x == midX
            then ' '
            else
              if Coordinate x y `Map.member` positionMap
                then 'R'
                else '.'
          | x <- [0 .. width - 1]
        ]
        | y <- [0 .. height - 1]
      ]

getQuadrant :: X -> Y -> Robot -> Maybe Int
getQuadrant width height robot =
  let midY = (height `div` 2)
      midX = (width `div` 2)
      p = position robot
      cmpX = compare (x p) midX
      cmpY = compare (y p) midY
   in case cmpX of
        EQ -> Nothing
        LT -> if cmpY == LT then Just 1 else if cmpY == GT then Just 3 else Nothing
        GT -> if cmpY == LT then Just 2 else if cmpY == GT then Just 4 else Nothing

part1 input width height n =
  product
    ( map
        length
        ( trace
            ( foldr
                ( \rs str ->
                    "Quadrant: "
                      ++ show (snd (head rs))
                      ++ " has "
                      ++ show (length rs)
                      ++ "\n"
                      ++ foldr (\r s -> r ++ "\n" ++ s) "" (showSpace width height (map fst rs))
                      ++ "\n\n"
                      ++ str
                )
                ""
                groupedByQuadrant
            )
            groupedByQuadrant
        )
    )
  where
    finalRobots = map (`advance` n) input
    robotQuadrants = mapMaybe (\r -> (\q -> Just (r, q)) =<< getQuadrant width height r) finalRobots
    groupedByQuadrant =
      groupBy
        (\(a, aQuad) (b, bQuad) -> aQuad == bQuad)
        (sortBy (\(a, aQuad) (b, bQuad) -> compare aQuad bQuad) robotQuadrants)

    advance :: Robot -> Int -> Robot
    advance (Robot p v) n =
      let x' = (x p + n * x v) `mod` width
          y' = (y p + n * y v) `mod` height
       in Robot (Coordinate x' y') v

part2 input = input

processInput :: String -> [Robot]
processInput contents = map parseRobot (lines contents)
  where
    parseRobot :: String -> Robot
    parseRobot line = case parse robotParser "" line of
      Left _ -> error "Error"
      Right r -> r

    intParser :: Parser Int
    intParser = do
      neg <- optionMaybe (char '-')
      str <- many1 digit
      let positive = read str :: Int
      return (maybe positive (const (-positive)) neg)

    coordinateParser :: Parser Coordinate
    coordinateParser = do
      x <- intParser
      void $ char ','
      Coordinate x <$> intParser

    robotParser :: Parser Robot
    robotParser = do
      void $ string "p="
      position <- coordinateParser
      void $ char ' '
      void $ string "v="
      Robot position <$> coordinateParser

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test 11 7 100) -- Expected: 12
  print (part1 input 101 103 100) -- Expected: 222062148
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?
