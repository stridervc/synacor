module Decoder
  ( decodeOpCode
  , numArgs
  ) where

decodeOpCode :: Int -> String
decodeOpCode 0  = "HALT"
decodeOpCode 6  = "JMP"
decodeOpCode 9  = "ADD"
decodeOpCode 19 = "OUT"
decodeOpCode 21 = "NOP"
decodeOpCode n  = error $ "Unknown opcode " <> show n

numArgs :: Int -> Int
numArgs 0   = 0
numArgs 6   = 1
numArgs 9   = 3
numArgs 19  = 1
numArgs 21  = 0
numArgs n   = error $ "Unknown opcode " <> show n

