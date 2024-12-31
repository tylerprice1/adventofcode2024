{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Data.Vector.Strict qualified as Vector
import Debug.Trace (traceShow, traceShowId)
import Text.Read (readMaybe)

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter fn [] = []
mapFilter fn (first : rest) = case fn first of
  Nothing -> restMapped
  Just v -> v : restMapped
  where
    restMapped = mapFilter fn rest

type StringVector = Vector.Vector String

type Id = Int

type BlockCount = Int

data Block
  = FileBlock Id
  | FreeBlock

instance Show Block where
  show FreeBlock = "."
  show (FileBlock id) = show id

data BlockSequence
  = File Id BlockCount
  | Free BlockCount

isBlockSequenceFree (Free _) = True
isBlockSequenceFree (File _ _) = False

instance Show BlockSequence where
  show (Free len) = replicate len '.'
  show (File id len) = concat (replicate len (show id))

getBlocksInBlockSequence :: BlockSequence -> [Block]
getBlocksInBlockSequence (Free len) = replicate len FreeBlock
getBlocksInBlockSequence (File id len) = replicate len (FileBlock id)

processInput :: String -> [BlockSequence]
processInput contents = files
  where
    sanitized = filter isDigit contents
    files =
      [ let len = read (ch : "") :: Int
         in if even id
              then File (id `div` 2) len
              else Free len
        | (id, ch) <- zip [0 ..] sanitized
      ]

checksum :: [Block] -> Int
checksum blocks = sum products
  where
    numbers =
      mapFilter
        ( \block -> case block of
            FreeBlock -> Nothing
            FileBlock id -> Just id
        )
        blocks
    products = zipWith (*) numbers [0 ..]

part1 :: [BlockSequence] -> Int
part1 input = checksum reordered
  where
    blocks = Vector.fromList (concatMap getBlocksInBlockSequence input)
    reordered = reorderBlocks blocks

    reorderBlocks :: Vector.Vector Block -> [Block]
    reorderBlocks files
      | Vector.null files = []
      | otherwise =
          let (first, rest) = (Vector.head files, Vector.tail files)
           in case first of
                FreeBlock ->
                  let (beginning, last) = getLastFileBlock rest
                   in case last of
                        FreeBlock -> error "Last should be file"
                        f -> f : reorderBlocks beginning
                f -> f : reorderBlocks rest
      where
        getLastFileBlock :: Vector.Vector Block -> (Vector.Vector Block, Block)
        getLastFileBlock files
          | Vector.null files = error "Empty vector"
          | otherwise = case last of
              FileBlock _ -> (beginning, last)
              FreeBlock -> getLastFileBlock beginning
          where
            (beginning, last') = Vector.splitAt (Vector.length files - 1) files
            last = Vector.head last'

part2 :: [BlockSequence] -> Int
part2 input = checksum (concatMap getBlocksInBlockSequence (traceShowId reordered))
  where
    blocks = Vector.fromList input
    reordered = reorderFiles blocks

    reorderFiles :: Vector.Vector BlockSequence -> [BlockSequence]
    reorderFiles blocks
      | Vector.length blocks <= 1 || Vector.all isBlockSequenceFree blocks = Vector.toList blocks
      | otherwise =
          let (first, rest) = (Vector.head blocks, Vector.tail blocks)
           in case first of
                Free freeLen ->
                  let free = first
                      (beginning, lastFile) = getLastFileBlock blocks
                      beginningExcludingFirst = Vector.tail beginning
                      File _ fileLen = lastFile
                      diff = freeLen - fileLen
                      remainingFree = Free diff
                   in case signum diff of
                        -1 -> traceShow ("Not enough", "free:", free, "beginning:", beginning, "lastFile:", lastFile, "reordered:", reorderFiles beginning, lastFile) reorderFiles beginning ++ [lastFile]
                        0 -> traceShow ("Exact     ", "free:", free, "beginning:", beginning, "lastFile:", lastFile, "reordered:", reorderFiles beginningExcludingFirst) (lastFile : reorderFiles beginningExcludingFirst)
                        1 -> traceShow ("Extra     ", "free:", free, "beginning:", beginning, "lastFile:", lastFile, "reordered:", reorderFiles (remainingFree `Vector.cons` beginningExcludingFirst)) (lastFile : reorderFiles (remainingFree `Vector.cons` beginningExcludingFirst))
                firstFile -> traceShow ("File      ", "firstFile:", firstFile, "reordered:", reorderFiles rest) (firstFile : reorderFiles rest)

    getLastFileBlock :: Vector.Vector BlockSequence -> (Vector.Vector BlockSequence, BlockSequence)
    getLastFileBlock blocks
      | Vector.null blocks = error "No file in blocks"
      | otherwise =
          let len = Vector.length blocks
              (beginning, last') = Vector.splitAt (len - 1) blocks
              last = Vector.head last'
           in case last of
                Free _ ->
                  let (newBeginning, newLast) = getLastFileBlock beginning
                      vconcat = (Vector.++)
                   in (newBeginning `vconcat` Vector.fromList [last], newLast)
                f -> (beginning, f)

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "----- Part 1 -----"
  -- print (part1 (processInput testFile)) -- 1928
  -- print (part1 (processInput inputFile)) -- 6359213660505
  putStrLn "----- Part 2 -----"
  print (part2 test) -- 2858
  -- print (part2 input) -- ?
  return ()
