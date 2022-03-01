module VMState
  ( VMState (..)
  , newVMState
  , newVMFromInts
  , newVMFromFile
  , stepVM
  , runVM
  , dumpRegisters
  ) where

import Data.Char (chr, ord)
import Data.Bits (shiftL)

type Register   = Int
type Operator   = Int

data VMState = VMState
  { vmMemory    :: [Int]      -- ^ 32768 Integers
  , vmRegisters :: [Register] -- ^ 8 Integers
  , vmStack     :: [Int]      -- ^ Unbounded stack
  , vmIP        :: Int        -- ^ Instruction Pointer
  , vmHalt      :: Bool       -- ^ Catch fire?
  } deriving (Eq, Show)

newVMState :: VMState
newVMState = VMState
  { vmMemory    = replicate 32768 0
  , vmRegisters = replicate 8 0
  , vmStack     = []
  , vmIP        = 0
  , vmHalt      = False
  }

newVMFromInts :: [Int] -> VMState
newVMFromInts input = newVMState { vmMemory = input <> fill }
  where fill  = replicate (32768 - length input) 0

charPairsToInts :: [Char] -> [Int]
charPairsToInts []        = []
charPairsToInts (a:b:rem) = (ord b `shiftL` 8 + ord a) : charPairsToInts rem
charPairsToInts _         = error "Invalid arguments to charPairsToInts"

newVMFromFile :: FilePath -> IO VMState
newVMFromFile f = do
  contents <- readFile f
  return $ newVMFromInts $ charPairsToInts contents

readMemory :: Int -> VMState -> Int
readMemory n state = vmMemory state !! n

writeMemory :: Int -> Int -> VMState -> VMState
writeMemory n val state = state { vmMemory = take n mem <> [val] <> drop (n+1) mem }
  where mem = vmMemory state

readRegister :: Int -> VMState -> Int
readRegister n state = vmRegisters state !! n

writeRegister :: Int -> Int -> VMState -> VMState
writeRegister n val state = state { vmRegisters = take n regs <> [val] <> drop (n+1) regs }
  where regs = vmRegisters state

writeMemoryOrRegister :: Int -> Int -> VMState -> VMState
writeMemoryOrRegister n val state
  | n < 32768 = writeMemory n val state
  | otherwise = writeRegister (n-32768) val state

-- get argument n for current operation
readArg :: Int -> VMState -> Int
readArg n state = readMemory (ip+1+n) state
  where ip = vmIP state

-- get argument n for current operation, converting to value in register if needed
argValue :: Int -> VMState -> Int
argValue n state = if arg >= 32768 then readRegister (arg-32768) state else arg
  where arg = readArg n state

stepVM :: VMState -> IO VMState
stepVM state = do
  case op of
    0   -> return $ (incIP 1) { vmHalt = True }
    9   -> return $ writeMemoryOrRegister argA ((valB + valC) `mod` 32768) $ incIP 4
    19  -> do
      putChar $ chr argA
      return $ incIP 2
    21  -> return $ incIP 1
    _   -> error $ "Unknown opcode " <> show op
  where incIP n = state { vmIP = vmIP state + n }
        ip      = vmIP state
        op      = readMemory ip state
        argA    = readArg 0 state
        argB    = readArg 1 state
        argC    = readArg 2 state
        valA    = argValue 0 state
        valB    = argValue 1 state
        valC    = argValue 2 state

runVM :: VMState -> IO VMState
runVM state = do
  state' <- stepVM state
  if vmHalt state' then return state' else runVM state'

dumpRegisters :: VMState -> IO ()
dumpRegisters state = do
  putStrLn $ "IP = " <> show (vmIP state)
  putStrLn $ "R0 = " <> show (readRegister 0 state)
  putStrLn $ "R1 = " <> show (readRegister 1 state)
  putStrLn $ "R2 = " <> show (readRegister 2 state)
  putStrLn $ "R3 = " <> show (readRegister 3 state)
  putStrLn $ "R4 = " <> show (readRegister 4 state)
  putStrLn $ "R5 = " <> show (readRegister 5 state)
  putStrLn $ "R6 = " <> show (readRegister 6 state)
  putStrLn $ "R7 = " <> show (readRegister 7 state)
  putStrLn $ "HALT = " <> show (vmHalt state)

