import Data.Bits (xor)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vector
import Debug.Trace (trace, traceShow, traceShowId)
import Numeric (readOct, showIntAtBase, showOct)
import Safe (atMay)
import Text.Parsec (char, digit, letter, many, many1, newline, optionMaybe, optional, parse, spaces, string)
import Text.Parsec.String (Parser)

newtype Register = Register {getValue :: Int}
  deriving (Show)

data Computer = Computer {getA :: Register, getB :: Register, getC :: Register}
  deriving (Show)

newtype Combo = Combo Int
  deriving (Show)

newtype Literal = Literal Int
  deriving (Show)

newtype NoOp = NoOp Int
  deriving (Show)

data Instruction
  = -- The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.
    ADV Combo
  | -- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
    BXL Literal
  | -- The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
    BST Combo
  | -- The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
    JNZ Literal
  | -- The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
    BXC NoOp
  | -- The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)
    OUT Combo
  | -- The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)
    BDV Combo
  | -- The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.)
    CDV Combo
  deriving (Show)

instructionToOpcode (ADV (Combo o)) = [0, o]

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
  deriving (Show)

setA :: Program -> Int -> Program
setA (Program (Computer (Register _) b c) ip is out) a = Program (Computer (Register a) b c) ip is out

setInstructionPointer :: Program -> InstructionPointer -> Program
setInstructionPointer (Program c _ is out) ip = Program c ip is out

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

    registerParser :: Parser Register
    registerParser = do
      _ <- string "Register"
      _ <- spaces
      _ <- letter
      _ <- char ':'
      _ <- spaces
      valueStr <- many digit
      _ <- newline
      let value = read valueStr :: Int
      return (Register value)

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

executeOne :: Program -> Maybe Program
executeOne program = case instructions `atMay` ip of
  Nothing -> Nothing
  Just (ADV (Combo operand)) -> Just (Program (Computer (Register (a `div` 2 ^ comboValue operand)) registerB registerC) nextIp instructions output)
  Just (BXL (Literal operand)) -> Just (Program (Computer registerA (Register (b `xor` operand)) registerC) nextIp instructions output)
  Just (BST (Combo operand)) -> Just (Program (Computer registerA (Register (comboValue operand `mod` 8)) registerC) nextIp instructions output)
  Just (JNZ (Literal operand))
    | a == 0 -> Just (program `setInstructionPointer` nextIp)
    | otherwise -> Just (program `setInstructionPointer` operand)
  Just (BXC (NoOp operand)) -> Just (Program (Computer registerA (Register (b `xor` c)) registerC) nextIp instructions output)
  Just (OUT (Combo operand)) -> Just (Program computer nextIp instructions (comboValue operand `mod` 8 : output))
  Just (BDV (Combo operand)) -> Just (Program (Computer registerA (Register (a `div` 2 ^ comboValue operand)) registerC) nextIp instructions output)
  Just (CDV (Combo operand)) -> Just (Program (Computer registerA registerB (Register (a `div` 2 ^ comboValue operand))) nextIp instructions output)
  where
    Program computer ip instructions output = program
    Computer registerA registerB registerC = computer
    nextIp = ip + 1
    Register a = registerA
    Register b = registerB
    Register c = registerC

    comboValue :: Int -> Int
    comboValue 0 = 0
    comboValue 1 = 1
    comboValue 2 = 2
    comboValue 3 = 3
    comboValue 4 = a
    comboValue 5 = b
    comboValue 6 = c
    comboValue 7 = error "Reserved"

execute :: Program -> Program
execute program = case instructions `atMay` ip of
  Nothing -> Program computer ip instructions (reverse output)
  Just (ADV (Combo operand)) -> execute (Program (Computer (Register (a `div` (2 ^ comboValue operand))) registerB registerC) nextIp instructions output)
  Just (BXL (Literal operand)) -> execute (Program (Computer registerA (Register (b `xor` operand)) registerC) nextIp instructions output)
  Just (BST (Combo operand)) -> execute (Program (Computer registerA (Register (comboValue operand `mod` 8)) registerC) nextIp instructions output)
  Just (JNZ (Literal operand))
    | a == 0 -> execute (program `setInstructionPointer` nextIp)
    | otherwise -> execute (program `setInstructionPointer` operand)
  Just (BXC (NoOp operand)) -> execute (Program (Computer registerA (Register (b `xor` c)) registerC) nextIp instructions output)
  Just (OUT (Combo operand)) -> execute (Program computer nextIp instructions (comboValue operand `mod` 8 : output))
  Just (BDV (Combo operand)) -> execute (Program (Computer registerA (Register (a `div` (2 ^ comboValue operand))) registerC) nextIp instructions output)
  Just (CDV (Combo operand)) -> execute (Program (Computer registerA registerB (Register (a `div` (2 ^ comboValue operand)))) nextIp instructions output)
  where
    Program computer ip instructions output = program
    Computer registerA registerB registerC = computer
    nextIp = ip + 1
    Register a = registerA
    Register b = registerB
    Register c = registerC

    comboValue :: Int -> Int
    comboValue 0 = 0
    comboValue 1 = 1
    comboValue 2 = 2
    comboValue 3 = 3
    comboValue 4 = a
    comboValue 5 = b
    comboValue 6 = c
    comboValue 7 = error "Reserved"

part1 = getOutput . execute

padLen :: Int -> Char -> String -> String
padLen len fill str =
  let l = length str
      diff = len - l
   in replicate diff fill ++ str

findA :: Program -> Int
findA input =
  foldr
    ( \(i, place) result ->
        traceShow
          ( i,
            showOct place "",
            result,
            take 5 (map (`showOct` "") range_0_999_octal),
            drop ((8 ^ 3) - 5) (map (`showOct` "") range_0_999_octal),
            take 5 (map (\i -> showOct (place + i * (place `div` (8 ^ 3))) "") range_0_999_octal),
            drop ((8 ^ 3) - 5) (map (\i -> showOct (place + i * (place `div` (8 ^ 3))) "") range_0_999_octal),
            findRange i (map (\i -> place + i * (place `div` (8 ^ 3))) range_0_999_octal) -- (execute (input `setA` (i * place)))
          )
          result
    )
    0
    (zip ints (map (8 ^) [0 ..]))
  where
    ints = instructionsToInts (getInstructions input)
    range_0_999_octal = [0 .. (8 ^ 3) - 1]

    findRange :: Int -> [Int] -> Maybe Int
    findRange i = find (\a -> i == (head . getOutput . execute) (input `setA` a))

part2 input = findA input

-- find
--   ( \a ->
--       let result = (getOutput . execute) (input `setA` a)
--        in trace
--             ( padLen 16 ' ' (show result)
--                 ++ " "
--                 ++ padLen 8 ' ' (showOct a "")
--                 ++ " "
--                 ++ padLen 8 ' ' (show a)
--                 ++ " "
--                 ++ show is
--             )
--             (is == result)
--   )
--   (trace (show start ++ " - " ++ show end) [1 .. 10000 {- [start, start + (end - start) `div` 700 .. end] -}]) -- ([start, start + 8 .. end])) -- [start, start + (2 ^ (length is)) .. end])
-- where
--   instructions = getInstructions input
--   is = instructionsToInts instructions
--   (start, end) = range is
--   range instructions =
--     let start = (8 ^ (length is - 1))
--      in (start, 8 * start - 1)

-- findA program

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  test2File <- readFile "./test2.txt"
  let test2 = processInput test2File

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn ("Test: " ++ show test)
  putStrLn ("Test2: " ++ show test2)
  putStrLn ("Input: " ++ show input)

  putStrLn "\n----- Part 1 -----"
  print (part1 test) -- Expected: 4,6,3,5,6,3,5,2,1,0
  print (part1 input) -- Expected: 6,7,5,2,1,3,5,1,7
  putStrLn "\n----- Part 2 -----"
  -- print (part2 test2) -- Expected: 117440
  print (part2 input) -- Expected: ?
