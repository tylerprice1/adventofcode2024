import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Regex

part1 = parseAll
  where
    parseAll :: String -> Maybe Int
    parseAll text =
      let (rest, result) = parseAll' text 0
       in if result == Just 0 then Nothing else result
      where
        parseAll' :: String -> Int -> (String, Maybe Int)
        parseAll' "" result = ("", Just result)
        parseAll' [ch] result = ([ch], Just result)
        parseAll' text result =
          let (rest, maybeInt) = parseOne text
           in case maybeInt of
                Nothing ->
                  let (newRest, maybeResult) = parseAll' rest result
                   in (newRest, Just (fromMaybe result maybeResult))
                Just justInt -> parseAll' rest (justInt + result)

part2 = parseAll
  where
    parseAll :: String -> Maybe Int
    parseAll text =
      let (rest, result) = parseAll' text 0 True
       in if result == Just 0 then Nothing else result
      where
        parseAll' :: String -> Int -> Bool -> (String, Maybe Int)
        parseAll' "" result _ = ("", Just result)
        parseAll' [ch] result _ = ([ch], Just result)
        parseAll' text result active =
          case text of
            ('d' : 'o' : '(' : ')' : rest) -> parseAll' rest result True
            ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : rest) -> parseAll' rest result False
            ('m' : 'u' : 'l' : '(' : rest) ->
              if active
                then
                  let (parseRest, maybeInt) = parseMul rest
                   in case maybeInt of
                        Nothing ->
                          let (newRest, maybeResult) = parseAll' parseRest result active
                           in (newRest, Just (fromMaybe result maybeResult))
                        Just justInt -> parseAll' parseRest (justInt + result) active
                else parseAll' rest result active
            (_ : other) -> parseAll' other result active

main :: IO ()
main = do
  test <- readFile "./test.txt"
  input <- readFile "./input.txt"

  putStrLn "----- Part 1 -----"
  print (part1 test)
  print (part1 input)

  putStrLn "----- Part 2 -----"
  print (part2 test)
  print (part2 input)

parseOne :: String -> (String, Maybe Int)
parseOne "" = ("", Nothing)
parseOne [ch] = ([ch], Nothing)
parseOne text =
  case text of
    ('m' : 'u' : 'l' : '(' : rest) -> parseMul rest
    (_ : rest) -> parseOne rest

parseMul :: String -> (String, Maybe Int)
parseMul text =
  let (firstNumberRest, maybeFirstNumber) = parseDigit text
   in case maybeFirstNumber of
        Nothing -> (firstNumberRest, Nothing)
        Just firstNumber -> case firstNumberRest of
          (',' : secondNumberStart) ->
            let (secondNumberRest, maybeSecondNumber) = parseDigit secondNumberStart
             in case maybeSecondNumber of
                  Nothing -> (secondNumberRest, Nothing)
                  Just secondNumber -> case secondNumberRest of
                    (')' : rest) -> (rest, Just (firstNumber * secondNumber))
                    (_ : rest) -> (rest, Nothing)
          (other : rest) -> (rest, Nothing)

parseDigit :: String -> (String, Maybe Int)
parseDigit text =
  let numberString = takeWhile isDigit text
      rest = drop (length numberString) text
   in case numberString of
        "" -> (rest, Nothing)
        str -> (rest, Just (read str :: Int))
