import Data.Bits (xor)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vector
import Text.Parsec (char, digit, letter, many, many1, newline, optionMaybe, optional, parse, spaces, string)
import Text.Parsec.String (Parser)

newtype Register = Register Int
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

type InstructionPointer = Int

data Program = Program {getComputer :: Computer, getInstructionPointer :: InstructionPointer, getInstructions :: Vector.Vector Instruction, getOutput :: [Int]}
  deriving (Show)

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
      return (Program computer 0 (Vector.fromList instructions) [])

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

execute :: Program -> Program
execute program = case (Vector.!?) instructions ip of
  Nothing -> Program computer ip instructions (reverse output)
  Just (ADV (Combo operand)) -> execute (Program (Computer (Register (a `div` 2 ^ comboValue operand)) registerB registerC) nextIp instructions output)
  Just (BXL (Literal operand)) -> execute (Program (Computer registerA (Register (b `xor` operand)) registerC) nextIp instructions output)
  Just (BST (Combo operand)) -> execute (Program (Computer registerA (Register (comboValue operand `mod` 8)) registerC) nextIp instructions output)
  Just (JNZ (Literal operand))
    | a == 0 -> Program computer ip instructions (reverse output)
    | otherwise -> execute (program `setInstructionPointer` operand)
  Just (BXC (NoOp operand)) -> execute (Program (Computer registerA (Register (b `xor` c)) registerC) nextIp instructions output)
  Just (OUT (Combo operand)) -> execute (Program computer nextIp instructions (comboValue operand `mod` 8 : output))
  Just (BDV (Combo operand)) -> execute (Program (Computer registerA (Register (a `div` 2 ^ comboValue operand)) registerC) nextIp instructions output)
  Just (CDV (Combo operand)) -> execute (Program (Computer registerA registerB (Register (a `div` 2 ^ comboValue operand))) nextIp instructions output)
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

-- runProgram computer program =

part1 = getOutput . execute

part2 input = input

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
