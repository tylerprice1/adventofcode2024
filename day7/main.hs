import Control.Exception (throw, throwIO)
import Data.Char
import Data.List (find)
import Data.Maybe (isJust)
import Debug.Trace (traceShow, traceShowId)
import Text.Parsec
import Text.PrettyPrint

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

data Expression
  = Literal Int
  | Add Expression Expression
  | Subtract Expression Expression
  | Multiply Expression Expression
  | Divide Expression Expression
  | Concat Expression Expression

showBinary operator lhs rhs = "(" ++ show lhs ++ " " ++ operator ++ " " ++ show rhs ++ ")"

instance Show Expression where
  show (Literal n) = show n
  show (Add lhs rhs) = showBinary "+" lhs rhs
  show (Subtract lhs rhs) = showBinary "-" lhs rhs
  show (Multiply lhs rhs) = showBinary "*" lhs rhs
  show (Divide lhs rhs) = showBinary "/" lhs rhs
  show (Concat lhs rhs) = showBinary "||" lhs rhs

type OperatorlessEquation = (Int, [Int])

evaluate expression = case expression of
  Literal n -> n
  Add lhs rhs -> evaluate lhs + evaluate rhs
  Subtract lhs rhs -> evaluate lhs - evaluate rhs
  Multiply lhs rhs -> evaluate lhs * evaluate rhs
  Divide lhs rhs -> evaluate lhs `div` evaluate rhs
  Concat lhs rhs -> read (show elhs ++ show erhs) :: Int
    where
      elhs = evaluate lhs
      erhs = evaluate rhs

generateBinaryExpression :: String -> Expression -> Expression -> Expression
generateBinaryExpression "+" = Add
generateBinaryExpression "-" = Subtract
generateBinaryExpression "*" = Multiply
generateBinaryExpression "/" = Divide
generateBinaryExpression "||" = Concat

generateCombinations :: [Int] -> [String] -> [Expression]
generateCombinations [] _ = []
generateCombinations [only] _ = [Literal only]
generateCombinations (l : rest) operators = concatMap (\o -> [generateBinaryExpression o lhs rhs | rhs <- rhss]) operators
  where
    lhs = Literal l
    rhss = generateCombinations rest operators ++ generateCombinations (reverse rest) operators

findExpression :: Int -> [Int] -> [String] -> Maybe Expression
findExpression result inputs operators = correct
  where
    expressions = generateCombinations inputs operators ++ generateCombinations (reverse inputs) operators
    evaluated = map (\e -> (evaluate e, e)) expressions
    correct = find (\e -> evaluate e == result) (traceShowId expressions)

part1 :: [OperatorlessEquation] -> Int
part1 equations = sum (map fst validEquations)
  where
    operators = ["+", "*"]
    expressions = map (\(result, inputs) -> findExpression result inputs operators) equations
    validEquations = filter (\(result, inputs) -> isJust (findExpression result inputs operators)) equations

part2 equations = sum (map fst validEquations)
  where
    operators = ["+", "*", "||"]
    expressions = map (\(result, inputs) -> findExpression result inputs operators) equations
    validEquations = filter (\(result, inputs) -> isJust (findExpression result inputs operators)) equations

parseLine :: String -> OperatorlessEquation
parseLine line = (result, numbers)
  where
    [resultStr, numbersStr] = split line ':'
    result = read resultStr :: Int

    numberStrs = filter (/= "") (split numbersStr ' ')
    numbers = map (\s -> read s :: Int) numberStrs

parseFile :: String -> [OperatorlessEquation]
parseFile contents = map parseLine (lines contents)

main = do
  testFile <- readFile "./test.txt"
  let test = parseFile testFile

  inputFile <- readFile "./input.txt"
  let input = parseFile inputFile

  putStrLn "----- Part 1 -----"
  print (part1 test) -- expected: 3749
  -- print (part1 input) -- expected: 3598800864292
  putStrLn "----- Part 2 -----"
  print (part2 test) -- expected: 11387
  -- print (part2 input) -- expected: ?
