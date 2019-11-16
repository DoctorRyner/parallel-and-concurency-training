module ParallelPlayground where

import           Control.Parallel.Strategies
import           Data.Function               ((&))
import           Data.List.Split

countSignChanges :: [Int] -> Int
countSignChanges [] = 0
countSignChanges xs = foldr (\(a, b) res -> if a >= 0 && b < 0 || a < 0 && b >= 0
                                            then succ res
                                            else res
                            ) 0 $ zip xs $ tail xs

countSignChangesWithoutZip :: [Int] -> Int
countSignChangesWithoutZip [] = 0
countSignChangesWithoutZip xs = fst $ foldl onSignCheck (0, head xs >= 0) xs
  where
    onSignCheck t@(count, switch) x = if switch /= (x >= count)
                                      then (succ count, not switch)
                                      else t

playground :: IO ()
playground = mempty

nonParallelCustom :: ([Int] -> Int) -> Int
nonParallelCustom = (&) ([0, -1] ++ [-1..60000000])

nonParallel, nonParallelWithoutZip :: Int
nonParallel           = nonParallelCustom countSignChanges
nonParallelWithoutZip = nonParallelCustom countSignChangesWithoutZip

parallelCustom :: ([Int] -> Int) -> Int
parallelCustom f =  runEval $ pure $ sum $ parMap rpar f $ chunksOf chunkSize ([0, -1] ++ [-1..60000000])
  where
    chunkSize = 1000

parallel, parallelWithoutZip :: Int
parallel           = parallelCustom countSignChanges
parallelWithoutZip = parallelCustom countSignChangesWithoutZip

