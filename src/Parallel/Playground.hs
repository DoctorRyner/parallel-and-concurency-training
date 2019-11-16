module Parallel.Playground where

import           Control.Parallel.Strategies
import           Data.Function               ((&))
import           Data.List.Split

-- The second algorithm to count sign changes in an array
countSignChanges :: [Integer] -> Int
countSignChanges [] = 0
countSignChanges xs = foldr (\(a, b) res -> if a >= 0 && b < 0 || a < 0 && b >= 0
                                            then succ res
                                            else res
                            ) 0 $ zip xs $ tail xs

-- The first, and as it turned out, the fastest algorithm that I came up with
countSignChangesWithoutZip :: [Integer] -> Int
countSignChangesWithoutZip [] = 0
countSignChangesWithoutZip xs = fromEnum $ fst $ foldl onSignCheck (0, head xs >= 0) xs
  where
    onSignCheck t@(count, switch) x = if switch /= (x >= count)
                                      then (succ count, not switch)
                                      else t

-- An example list to test algorithms
testList :: [Integer]
testList = [0, -1] ++ [-1..100000000] ++ [ -1, -7 ]

-- Non parallel functions
nonParallelCustom :: ([Integer] -> Int) -> Int
nonParallelCustom = (&) testList

nonParallel, nonParallelWithoutZip :: Int
nonParallel           = nonParallelCustom countSignChanges
nonParallelWithoutZip = nonParallelCustom countSignChangesWithoutZip

-- Parallel functions
parallelCustom :: ([Integer] -> Int) -> Int
parallelCustom f =  runEval $ pure $ sum $ parMap rpar f $ chunksOf chunkSize testList
  where
    chunkSize = 10000

parallel, parallelWithoutZip :: Int
parallel           = parallelCustom countSignChanges
parallelWithoutZip = parallelCustom countSignChangesWithoutZip

