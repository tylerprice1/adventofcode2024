import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Data.Vector.Strict qualified as Vector
import Debug.Trace (traceShow, traceShowId)
import Text.Read (readMaybe)

type StringVector = Vector.Vector String

type Block = String

data BlockSequence
  = File [Block]
  | Free Int

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter fn [] = []
mapFilter fn (first : rest) = case fn first of
  Nothing -> restMapped
  Just v -> v : restMapped
  where
    restMapped = mapFilter fn rest

reorderBlocks :: StringVector -> [String]
reorderBlocks files
  | Vector.null files = []
  | otherwise =
      let (first', rest) = Vector.splitAt 1 files
          first = Vector.head first'
       in case first of
            "." ->
              let (beginning, last) = getLastBlock rest
               in last : reorderBlocks beginning
            num -> num : reorderBlocks rest
  where
    getLastBlock :: StringVector -> (StringVector, String)
    getLastBlock chars
      | Vector.null chars = error "Empty vector"
      | all isDigit last = (beginning, last)
      | otherwise = getLastBlock beginning
      where
        (beginning, last') = Vector.splitAt (Vector.length chars - 1) chars
        last = Vector.head last'

reorderFiles :: StringVector -> [String]
reorderFiles files
  | Vector.null files = []
  | otherwise = []

part1 :: StringVector -> Int
part1 input = sum products
  where
    reordered = reorderBlocks input
    numbers = mapFilter (\s -> readMaybe s :: Maybe Int) reordered
    products = zipWith (*) numbers [0 ..]

part2 :: StringVector -> Int
part2 input = 0

processFile :: String -> StringVector
processFile contents = Vector.fromList (concat (readFileLength idDigitPairs))
  where
    sanitized = filter isDigit contents
    ids = map (`div` 2) [0 ..]
    idDigitPairs = zip ids sanitized

    readFileLength :: [(Int, Char)] -> [[String]]
    readFileLength [] = []
    readFileLength ((id, ch) : rest) = block : readFreeSpace rest
      where
        len = read (ch : "") :: Int
        idStr = show id
        block = replicate len idStr

    readFreeSpace :: [(Int, Char)] -> [[String]]
    readFreeSpace [] = []
    readFreeSpace ((_, ch) : rest) = block : readFileLength rest
      where
        len = read (ch : "") :: Int
        block = replicate len "."

main = do
  testFile <- readFile "./test.txt"
  let test = processFile testFile

  inputFile <- readFile "./input.txt"
  let input = processFile inputFile

  putStrLn "----- Part 1 -----"
  print (part1 test) -- 1928
  print (part1 input) -- 6359213660505
  putStrLn "----- Part 2 -----"
  print (part2 test) -- 2858
  print (part2 input) -- ?
  return ()
