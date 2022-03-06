module Decoder
  ( decodeOpCode
  , numArgs
  , hex
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
decodeOpCode 10 = "MUL"
decodeOpCode 11 = "MOD"
decodeOpCode 12 = "AND"
decodeOpCode 13 = "OR"
decodeOpCode 14 = "NOT"
decodeOpCode 15 = "RMEM"
decodeOpCode 16 = "WMEM"
decodeOpCode 17 = "CALL"
decodeOpCode 18 = "RET"
decodeOpCode 19 = "OUT"
decodeOpCode 21 = "NOP"
decodeOpCode n  = show n

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
numArgs 10  = 3
numArgs 11  = 3
numArgs 12  = 3
numArgs 13  = 3
numArgs 14  = 2
numArgs 15  = 2
numArgs 16  = 2
numArgs 17  = 1
numArgs 18  = 0
numArgs 19  = 1
numArgs 21  = 0
numArgs _   = 0

-- convert int to hex string
hex' :: Int -> String
hex' n
  | n <= 9    = show n
  | n <= 15   = "abcdef" !! (n-10) : ""
  | otherwise = hex' (n `div` 16) <> hex' (n `mod` 16)

-- preceded by "0x" and padded with "0" to 4 digits
hex :: Int -> String
--hex n = take 2 padded <> " " <> drop 2 padded
hex n = padded
  where hexed   = hex' n
        pad     = replicate (4 - length hexed) '0'
        padded  = pad <> hexed
