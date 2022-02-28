module Main where

import VMState

main :: IO ()
main = do
  runVM $ newVMFromInts [ 9, 32768, 32769, 4, 19, 32768 ]
