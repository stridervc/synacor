module Decoder
  ( decodeOpCode
  , numArgs
  ) where

decodeOpCode :: Int -> String
decodeOpCode 0  = "HALT"
decodeOpCode 1  = "SET"
decodeOpCode 2  = "PUSH"
decodeOpCode 3  = "POP"
decodeOpCode 4  = "EQ"
decodeOpCode 5  = "GT"
decodeOpCode 6  = "JMP"
decodeOpCode 7  = "JT"
decodeOpCode 8  = "JF"
decodeOpCode 9  = "ADD"
decodeOpCode 19 = "OUT"
decodeOpCode 21 = "NOP"
decodeOpCode n  = "? " <> show n

numArgs :: Int -> Int
numArgs 0   = 0
numArgs 1   = 2
numArgs 2   = 1
numArgs 3   = 1
numArgs 4   = 3
numArgs 5   = 3
numArgs 6   = 1
numArgs 7   = 2
numArgs 8   = 2
numArgs 9   = 3
numArgs 19  = 1
numArgs 21  = 0
numArgs _   = 0

