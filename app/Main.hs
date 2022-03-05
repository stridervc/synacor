module Main where

import VMState
import BrickRunner

main :: IO ()
main = do
  vm <- newVMFromFile "download/challenge.bin"
  vm' <- brickRun vm
  putStrLn ""
