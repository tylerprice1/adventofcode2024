import Data.Bits (xor)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
-- import Numeric (readOct, showIntAtBase, showOct)

-- import Debug.Trace (trace, traceShowId)
import Numeric (showOct)
import Safe (atMay)
import Text.Parsec (char, digit, letter, many, many1, newline, optionMaybe, optional, parse, spaces, string)
import Text.Parsec.String (Parser)

trace a b = b

padStart :: Int -> Char -> String -> String
padStart len fill str =
  let l = length str
      diff = len - l
   in replicate diff fill ++ str

data Computer = Computer {getA :: Int, getB :: Int, getC :: Int}

instance Show Computer where
  show (Computer a b c) = "Computer(" ++ padStart len ' ' (showOct a "") ++ ", " ++ padStart len ' ' (showOct b "") ++ ", " ++ padStart len ' ' (showOct c "") ++ ")"
    where
      len = max 4 (length (show (max a (max b c))))

newtype Combo = Combo Int

instance Show Combo where
  show (Combo i) = show i

newtype Literal = Literal Int

instance Show Literal where
  show (Literal i) = show i

newtype NoOp = NoOp Int

instance Show NoOp where
  show (NoOp i) = show i

data Instruction
  = ADV Combo
  | BXL Literal
  | BST Combo
  | JNZ Literal
  | BXC NoOp
  | OUT Combo
  | BDV Combo
  | CDV Combo
  deriving (Show)

instructionsToInts :: [Instruction] -> [Int]
instructionsToInts [] = []
instructionsToInts ((ADV (Combo o)) : is) = 0 : o : instructionsToInts is
instructionsToInts ((BXL (Literal o)) : is) = 1 : o : instructionsToInts is
instructionsToInts ((BST (Combo o)) : is) = 2 : o : instructionsToInts is
instructionsToInts ((JNZ (Literal o)) : is) = 3 : o : instructionsToInts is
instructionsToInts ((BXC (NoOp o)) : is) = 4 : o : instructionsToInts is
instructionsToInts ((OUT (Combo o)) : is) = 5 : o : instructionsToInts is
instructionsToInts ((BDV (Combo o)) : is) = 6 : o : instructionsToInts is
instructionsToInts ((CDV (Combo o)) : is) = 7 : o : instructionsToInts is

type InstructionPointer = Int

data Program = Program {getComputer :: Computer, getInstructionPointer :: InstructionPointer, getInstructions :: [Instruction], getOutput :: [Int]}

instance Show Program where
  show (Program computer instructionPointer instructions output) =
    "Program( "
      ++ show computer
      ++ ", "
      ++ padStart 12 ' ' (show (instructions `atMay` instructionPointer))
      ++ ", "
      ++ show output
      ++ " )"

{-# INLINE setA #-}
setA :: Computer -> Int -> Computer
setA (Computer _ b c) a = Computer a b c

{-# INLINE setB #-}
setB :: Computer -> Int -> Computer
setB (Computer a _ c) b = Computer a b c

{-# INLINE setC #-}
setC :: Computer -> Int -> Computer
setC (Computer a b _) = Computer a b

{-# INLINE setComputer #-}
setComputer :: Program -> Computer -> Program
setComputer (Program _ ip is out) c = Program c ip is out

{-# INLINE setInstructionPointer #-}
setInstructionPointer :: Program -> InstructionPointer -> Program
setInstructionPointer (Program c _ is out) ip = Program c ip is out

execute :: Program -> Program
execute program = case {- trace (show program) -} instructions `atMay` ip of
  Nothing -> error "No instruction"
  Just (ADV (Combo operand)) -> Program (computer `setA` aDiv operand) nextIp instructions output
  Just (BDV (Combo operand)) -> Program (computer `setB` aDiv operand) nextIp instructions output
  Just (CDV (Combo operand)) -> Program (computer `setC` aDiv operand) nextIp instructions output
  Just (BST (Combo operand)) -> Program (computer `setB` (comboValue operand `mod` 8)) nextIp instructions output
  Just (OUT (Combo operand)) -> Program computer nextIp instructions (comboValue operand `mod` 8 : output)
  Just (BXC _) -> Program (computer `setB` (b `xor` c)) nextIp instructions output
  Just (BXL (Literal operand)) -> Program (computer `setB` (b `xor` operand)) nextIp instructions output
  Just (JNZ (Literal operand))
    | a == 0 -> program `setInstructionPointer` nextIp
    | otherwise -> program `setInstructionPointer` operand
  where
    !(Program (!computer) (!ip) (!instructions) (!output)) = program
    !(Computer (!a) (!b) (!c)) = computer
    !nextIp = ip + 1

    aDiv :: Int -> Int
    aDiv operand = a `div` 2 ^ comboValue operand

    comboValue :: Int -> Int
    comboValue 0 = 0
    comboValue 1 = 1
    comboValue 2 = 2
    comboValue 3 = 3
    comboValue 4 = a
    comboValue 5 = b
    comboValue 6 = c
    comboValue 7 = error "Reserved"

run :: Program -> Program
run program = case {- trace (show program) -} instructions `atMay` ip of
  Nothing -> Program computer ip instructions (reverse output)
  _ -> run (execute program)
  where
    !(Program computer (!ip) (!instructions) output) = program

buildUp :: Program -> Int -> Maybe Program
buildUp program depth
  | depth > length ints = Nothing
  | otherwise =
      find
        (\p -> isJust (buildUp p (depth + 1)))
        ( trace
            (show depth ++ " filtered: \n" ++ foldr (\p s -> show (getA (getComputer p)) ++ " " ++ show (run p) ++ "\n" ++ s) "\n" programs)
            allPrograms
        )
  where
    values = map (\i -> i * 8 ^ (depth - 1)) [0 .. 7]
    as = map (\i -> i + getA computer) values
    allPrograms = map (\a -> program `setComputer` (computer `setA` a)) as
    programs =
      filter
        (\p -> getOutput (run p) == take depth ints)
        ( trace
            (show depth ++ " all: " ++ show (take depth ints) ++ "\n" ++ foldr (\p s -> show p ++ " " ++ show (run p) ++ "\n" ++ s) "\n" allPrograms)
            allPrograms
        )
    Program computer ip instructions output = program
    ints = instructionsToInts instructions

part1 :: Program -> [Int]
part1 = getOutput . run

-- part2 :: Program -> Maybe Int
part2 program = buildUp (program `setComputer` (getComputer program `setA` 0)) 1

-- trace
--   (showOct start "" ++ " - " ++ showOct end "" ++ "\n" ++ foldr (\i s -> show i ++ s) "" instructions)
--   (find (\a -> instructions == (getOutput . run) (program `setComputer` (getComputer program `setA` a))) [88782561745752 - 1_000_000_000 .. 88782561745752 + 1_000_000_000])
-- where
--   instructions = instructionsToInts (getInstructions program)
--   (start, end) = range instructions

--   range :: [Int] -> (Int, Int)
--   range instructions =
--     let start = (8 ^ (length instructions - 1))
--      in (start, 8 * start - 1)

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  test2File <- readFile "./test2.txt"
  let test2 = processInput test2File

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "\n----- Part 1 -----"
  -- print (part1 test) -- Expected: 4,6,3,5,6,3,5,2,1,0
  print (part1 input) -- Expected: 6,7,5,2,1,3,5,1,7
  --
  putStrLn "\n----- Part 2 -----"
  -- print (part2 test2) -- Expected: 117440
  print (part2 input) -- Expected: ?

processInput :: String -> Program
processInput content = case parse parser "" content of
  Left parseError -> error (show parseError)
  Right r -> r
  where
    parser :: Parser Program
    parser = do
      a <- registerParser
      b <- registerParser
      c <- registerParser
      _ <- newline
      let computer = Computer a b c
      programParser computer

    registerParser :: Parser Int
    registerParser = do
      _ <- string "Register"
      _ <- spaces
      _ <- letter
      _ <- char ':'
      _ <- spaces
      valueStr <- many digit
      _ <- newline
      let value = read valueStr :: Int
      return value

    programParser :: Computer -> Parser Program
    programParser computer = do
      _ <- string "Program"
      _ <- char ':'
      _ <- spaces
      instructions <- instructionsParser
      return (Program computer 0 instructions [])

    instructionsParser :: Parser [Instruction]
    instructionsParser = do
      i <- instructionParser
      _ <- optional (char ',')
      rest <- optionMaybe instructionsParser
      return (i : fromMaybe [] rest)

    instructionParser :: Parser Instruction
    instructionParser = do
      iStr <- digit
      let opcode = read (iStr : "") :: Int
      _ <- char ','
      operandStr <- digit
      let operand = read (operandStr : "") :: Int
      return
        ( case opcode of
            0 -> ADV (Combo operand)
            1 -> BXL (Literal operand)
            2 -> BST (Combo operand)
            3 -> JNZ (Literal operand)
            4 -> BXC (NoOp operand)
            5 -> OUT (Combo operand)
            6 -> BDV (Combo operand)
            7 -> CDV (Combo operand)
        )
