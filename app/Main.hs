module Main where

import VMState
import BrickRunner
import Data.List (permutations)

-- _ + _ * _^2 + _^3 - _ = 399
calc :: [Int] -> Int
calc ns = n0 + (n1 * (n2^2)) + (n3^3) - n4
  where n x = ns !! x
        n0  = n 0
        n1  = n 1
        n2  = n 2
        n3  = n 3
        n4  = n 4

main :: IO ()
main = do
  let nums  = [ 2, 3, 5, 7, 9 ]
  let ans   = head $ filter (\ns -> calc ns == 399) $ permutations nums
  
  -- print ans

  vm <- newVMFromFile "download/challenge.bin"
  vm' <- brickRun vm
  putStrLn ""
