import Data.Char (isDigit)
import Data.Matrix qualified as Matrix
import Debug.Trace (trace, traceShow, traceShowId)

type NumType = Int

type Token = NumType

type X = NumType

type Y = NumType

data Position = Position {x :: X, y :: Y}

data Button = Button {dx :: X, dy :: Y}

data ClawMachine = ClawMachine Button Button Position

isInt n = n == fromInteger (floor n) && n == fromInteger (ceiling n)

toFloat n = fromIntegral n :: Float

play :: NumType -> NumType -> ClawMachine -> Int
play aCost bCost (ClawMachine a b prize) =
  if expectedPrizes == calculatedPrizes then aCost * aPresses + bCost * bPresses else 0
  where
    xRow = [dx a, dx b, x prize]
    yRow = [dy a, dy b, y prize]
    intMatrix = Matrix.fromLists [xRow, yRow]
    matrix = Matrix.fromLists [map toFloat xRow, map toFloat yRow]
    rref = either error id (Matrix.rref matrix)

    aPresses = round (Matrix.getElem 1 3 rref)
    bPresses = round (Matrix.getElem 2 3 rref)

    expectedPrizes = Matrix.fromLists [[x prize], [y prize]]
    calculatedPrizes = Matrix.fromLists [[dx a, dx b], [dy a, dy b]] `Matrix.multStd` Matrix.fromLists [[aPresses], [bPresses]]

part1 machines = sum (map (play 3 1) machines)

part2 input = ""

processInput :: String -> [ClawMachine]
processInput contents = parse (lines contents)
  where
    parse :: [String] -> [ClawMachine]
    parse [] = []
    parse [a] = error "Too few elements"
    parse [a, b] = error "Too few elements"
    parse [a, b, prize] = [parseClawMachine a b prize]
    parse s =
      let (curr, remaining) = break null s
       in case curr of
            [a, b, prize] -> parseClawMachine a b prize : parse (dropWhile null remaining)
            _ -> error ("Wrong element count " ++ show curr)

    parseClawMachine :: String -> String -> String -> ClawMachine
    parseClawMachine aButton bButton prize = ClawMachine (parseButton aButton) (parseButton bButton) (parsePrize prize)

    parseButton :: String -> Button
    parseButton s = let (x, y) = parseXY s in Button x y

    parsePrize :: String -> Position
    parsePrize s = let (x, y) = parseXY s in Position x y

    parseXY :: String -> (X, Y)
    parseXY s = case span isDigit (dropWhile (not . isDigit) s) of
      ([], _) -> error ("No X for " ++ s)
      (xStr, s') ->
        case span isDigit (dropWhile (not . isDigit) s') of
          ([], _) -> error ("No Y for " ++ s)
          (yStr, s'') -> (read xStr :: NumType, read yStr :: NumType)

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: 480
  print (part1 input) -- Expected: ?
  -- putStrLn "\n----- Part 2 -----"
  -- print (part2 test) -- Expected: ?
  -- print (part2 input) -- Expected: ?
