module Main where

import VMState

main :: IO ()
main = do
  --let vm = newVMFromInts [ 9, 32768, 32769, 4, 19, 32768 ]
  vm <- newVMFromFile "download/challenge.bin"
  putStrLn ""
  vm' <- runVM vm
  putStrLn ""
