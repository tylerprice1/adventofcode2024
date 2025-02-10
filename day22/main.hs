{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

import Control.DeepSeq (NFData, deepseq)
import Data.Bits (Bits (xor))
import Data.Function (on)
import Data.List (maximumBy, sortBy, sortOn, zip4)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug.Trace (trace, traceShow, traceShowId, traceWith)
import GHC.Generics (Generic)
import GHC.List (foldr')

newtype Secret = Secret Integer
  deriving (Bits, Enum, Eq, Integral, Num, Ord, Real, Generic, NFData)

instance Show Secret where
  show (Secret i) = show i

newtype Price = Price Integer
  deriving (Eq, Ord, Bits, Enum, Integral, Num, Real, Generic, NFData)

instance Show Price where
  show (Price i) = show i

getPrice :: Secret -> Price
getPrice (Secret i) = Price (i `mod` 10)

evolve :: (Bits a, Integral a) => a -> a
evolve start =
  let -- Calculate the result of multiplying the secret number by 64.
      -- Then, mix this result into the secret number.
      -- Finally, prune the secret number.
      a = prune ((start * 64) `mix` start)
      -- Calculate the result of dividing the secret number by 32.
      -- Round the result down to the nearest integer.
      -- Then, mix this result into the secret number.
      -- Finally, prune the secret number.
      b = prune ((a `div` 32) `mix` a)
      -- Calculate the result of multiplying the secret number by 2048.
      -- Then, mix this result into the secret number.
      -- Finally, prune the secret number.
      c = prune ((b * 2048) `mix` b)
   in c
  where
    mix :: (Bits a) => a -> a -> a
    mix a b = a `xor` b

    prune :: (Integral a) => a -> a
    prune a = a `mod` 16777216

generate :: (Bits a, Integral a) => a -> [a]
generate start = start : generate (evolve start)

getDiffMap :: (Num a, Ord a, Show a) => Secret -> [a] -> Map.Map (a, a, a, a) [a] -> Map.Map (a, a, a, a) [a]
getDiffMap secret prices initialMap =
  fst
    ( foldl
        ( \(m, visited) (i, price) ->
            let _4 = priceAt (i - 4)
                _3 = priceAt (i - 3)
                _2 = priceAt (i - 2)
                _1 = priceAt (i - 1)
                fourTuple =
                  ( _3 - _4,
                    _2 - _3,
                    _1 - _2,
                    price - _1
                  )
             in if fourTuple `Set.member` visited
                  then
                    if fourTuple == (-1, 1, -3, 3)
                      then
                        traceShow (secret, i, price, "duplicate") (m, visited)
                      else (m, visited)
                  else
                    let visited' = fourTuple `Set.insert` visited
                        result = case Map.lookup fourTuple m of
                          Nothing -> (Map.insert fourTuple [price] m, visited')
                          Just ps -> (Map.insert fourTuple (price : ps) m, visited')
                     in if fourTuple == (-1, 1, -3, 3)
                          then traceShow (secret, i, price, Map.lookup fourTuple (fst result)) result
                          else result
        )
        (initialMap, Set.empty)
        (Vector.drop 4 vector)
    )
  where
    vector = Vector.fromList (zip [0 ..] prices)
    priceAt i = snd ((Vector.!) vector i)

part1 :: [Secret] -> Secret
part1 input = sum (map ((!! 2000) . generate) input)

part2 :: [Secret] -> Price
part2 input =
  let secrets = map (take 2001 . generate) input
      diffMap =
        foldr
          ( \secret m ->
              ( (\prices -> getDiffMap secret prices m)
                  . map getPrice
                  . (take 2001 . generate)
              )
                (secret)
          )
          Map.empty
          input
      mapList = Map.toList diffMap
      withSums = map (\(t, p) -> (t, sum p, p)) mapList
      getSum (_, s, _) = s
   in {- traceShow
        (map (\(a, b, _) -> (a, b)) (reverse (sortOn getSum withSums))) -}
      (getSum (maximumBy (compare `on` getSum) (withSums)))

main :: IO ()
main = do
  testFile <- readFile "./test.txt"
  let test = processInput testFile

  test2File <- readFile "./test2.txt"
  let test2 = processInput test2File

  inputFile <- readFile "./input.txt"
  let input = processInput inputFile

  -- putStrLn "\n----- Part 1 -----"
  -- print (part1 test, 37327623) -- Expected: 37327623
  -- print (part1 input, 13429191512) -- Expected: 13429191512
  putStrLn "\n----- Part 2 -----"
  -- print (part2 test, 24) -- Expected: 23
  -- print (part2 test2, 23) -- Expected: 23
  print (part2 input, 1582) -- Expected: 1582
  -- print (part2 [123], -1) -- Expected: ?
  -- print (part2 [2021, 5017, 19751], 27) -- Expected: ?
  -- print (part2 [5053, 10083, 11263], 27) -- Expected: ?
  -- print (part2 [123]) -- Expected: ?

processInput :: String -> [Secret]
processInput contents = map (\l -> Secret (read l :: Integer)) (lines contents)
