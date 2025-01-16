import Data.Maybe (fromMaybe)
import Text.Parsec (char, digit, letter, many, many1, newline, optionMaybe, optional, parse, spaces, string)
import Text.Parsec.String (Parser)

part1 input = input

part2 input = input

data Register = Register Char Int
  deriving (Show)

data Computer = Computer Register Register Register Program
  deriving (Show)

newtype Program = Program [Int]
  deriving (Show)

processInput :: String -> Computer
processInput content = case parse parser "" content of
  Left parseError -> error (show parseError)
  Right r -> r
  where
    parser :: Parser Computer
    parser = do
      a <- registerParser
      b <- registerParser
      c <- registerParser
      _ <- newline
      Computer a b c <$> programParser

    registerParser :: Parser Register
    registerParser = do
      _ <- string "Register"
      _ <- spaces
      name <- letter
      _ <- char ':'
      _ <- spaces
      valueStr <- many digit
      _ <- newline
      let value = read valueStr :: Int
      return (Register name value)

    programParser :: Parser Program
    programParser = do
      _ <- string "Program"
      _ <- char ':'
      _ <- spaces
      Program <$> instructionsParser

    instructionsParser :: Parser [Int]
    instructionsParser = do
      iStr <- digit
      let i = read (iStr : "") :: Int
      _ <- optional (char ',')
      rest <- optionMaybe (instructionsParser)
      return (i : fromMaybe [] rest)

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: ?
  print (part1 input) -- Expected: ?
  putStrLn "\n----- Part 2 -----"
  print (part2 test) -- Expected: ?
  print (part2 input) -- Expected: ?
