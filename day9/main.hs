{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
import Data.Char (isDigit, isSpace)
import Data.List
import Data.Maybe (fromJust, fromMaybe)
import Data.Vector.Strict qualified as Vector
import Debug.Trace (trace, traceShow, traceShowId)
import Text.Read (readMaybe)

splitOnFirst :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitOnFirst _ [] = Nothing
splitOnFirst predicate (first : rest) =
  if predicate first
    then Just ([], first, rest)
    else case splitOnFirst predicate rest of
      Nothing -> Nothing
      Just (restFalse, match, restTrue) -> Just (first : restFalse, match, restTrue)

splitVectorAtLast v =
  let (beginning, last') = Vector.splitAt (Vector.length v - 1) v
   in (beginning, Vector.head last')

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

showBlockSequences :: Vector.Vector BlockSequence -> String
showBlockSequences = Vector.foldr (\block str -> show block ++ str) ""

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
            FreeBlock -> Just 0
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
part2 input = checksum (concatMap getBlocksInBlockSequence reordered)
  where
    reordered = reorder (Vector.fromList input)

    isBlockSequenceFreeOfAtLeastLength (Free freeLen) len = freeLen >= len
    isBlockSequenceFreeOfAtLeastLength _ _ = False

    concatv = (Vector.++)
    prependv = Vector.cons
    appendv = Vector.snoc

    mergeConsecutiveFree v
      | Vector.length v <= 1 = v
      | otherwise =
          let (first, firstTail) = fromJust (Vector.uncons v)
              (second, secondTail) = fromJust (Vector.uncons firstTail)
           in case (first, second) of
                (File _ _, _) -> first `prependv` mergeConsecutiveFree firstTail
                (_, File _ _) -> first `prependv` mergeConsecutiveFree firstTail
                (Free a, Free b) -> Free (a + b) `prependv` mergeConsecutiveFree secondTail

    reorder :: Vector.Vector BlockSequence -> Vector.Vector BlockSequence
    reorder blocks = case Vector.unsnoc blocks of
      Nothing -> blocks
      Just (init, last) -> case last of
        Free _ -> reorder init `appendv` last
        File id fileLen ->
          case Vector.findIndex (`isBlockSequenceFreeOfAtLeastLength` fileLen) init of
            Nothing -> reorder init `appendv` last
            Just index ->
              let left = Vector.take index init
                  Free freeLen = (Vector.!) init index
                  right = Vector.drop (index + 1) init
                  diff = freeLen - fileLen
                  movedBlocks = Vector.fromList (if diff > 0 then [last, Free diff] else [last])
                  newBlocks = mergeConsecutiveFree (left `concatv` movedBlocks `concatv` right `appendv` Free fileLen)
               in reorder newBlocks

main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  putStrLn "----- Part 1 -----"
  print (part1 (processInput testFile)) -- 1928
  print (part1 (processInput inputFile)) -- 6359213660505
  putStrLn "----- Part 2 -----"
  print (part2 test) -- 2858
  print (part2 input) -- ?
  return ()
