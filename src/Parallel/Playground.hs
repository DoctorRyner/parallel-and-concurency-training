module Parallel.Playground where

import           Control.Parallel.Strategies
import           Data.Function               ((&))
import           Data.List.Split

-- The second algorithm to count sign changes in an array
countSignChanges :: Integral a =>  [a] -> a
countSignChanges [] = 0
countSignChanges xs = foldr (\(a, b) res -> if a >= 0 && b < 0 || a < 0 && b >= 0
                                            then succ res
                                            else res
                            ) 0 $ zip xs $ tail xs

-- The first, and as it turned out, the fastest algorithm that I came up with
countSignChangesWithoutZip :: Integral a => [a] -> a
countSignChangesWithoutZip [] = 0
countSignChangesWithoutZip xs = fst $ foldl onSignCheck (0, head xs >= 0) xs
  where
    onSignCheck t@(count, switch) x = if switch /= (x >= count)
                                      then (succ count, not switch)
                                      else t

-- An example list to test algorithms
testList :: Integral a => [a]
testList = [0, -1] ++ [-1..600000000] ++ [ -1, -7 ]

-- Non parallel functions
nonParallelCustom :: Integral a => ([a] -> a) -> a
nonParallelCustom = (&) testList

nonParallel, nonParallelWithoutZip :: Integral a => a
nonParallel           = nonParallelCustom countSignChanges
nonParallelWithoutZip = nonParallelCustom countSignChangesWithoutZip

-- Parallel functions
parallelCustom :: Integral a => ([a] -> a) -> a
parallelCustom f =  runEval $ pure $ sum $ parMap rpar f $ chunksOf chunkSize ([0, -1] ++ [-1..60000000])
  where
    chunkSize = 10000

parallel, parallelWithoutZip :: Integral a => a
parallel           = parallelCustom countSignChanges
parallelWithoutZip = parallelCustom countSignChangesWithoutZip

