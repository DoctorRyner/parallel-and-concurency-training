module Main where

import           Parallel.Playground
import           System.Environment

main :: IO ()
main = do
    args <- getArgs

    putStrLn $ case args of
        [x] -> show $ case x of
                "parallel"           -> parallel :: Int
                "parallelWithoutZip" -> parallelWithoutZip
                "nonParallel"        -> nonParallel
                _                    -> nonParallelWithoutZip
        _   -> "You must provide just one argument: " ++ concat [ "parallel, "
                                                                , "parallelWithoutZip, "
                                                                , "nonParallel or "
                                                                , "nonParallelWithoutZip"
                                                                ]
