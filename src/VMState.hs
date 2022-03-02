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
import Control.Monad.State

type Register   = Int
type Operator   = Int

type VMUpdater = StateT VMState IO

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

readMemory :: Int -> VMUpdater Int
readMemory n = do
  state <- get
  return $ vmMemory state !! n

writeMemory :: Int -> Int -> VMUpdater ()
writeMemory n val = do
  state <- get
  let mem = vmMemory state
  put $ state { vmMemory = take n mem <> [val] <> drop (n+1) mem }

readRegister :: Int -> VMUpdater Int
readRegister n = do
  state <- get
  return $ vmRegisters state !! n

writeRegister :: Int -> Int -> VMUpdater ()
writeRegister n val = do
  state <- get
  let regs = vmRegisters state
  put $ state { vmRegisters = take n regs <> [val] <> drop (n+1) regs }

writeMemoryOrRegister :: Int -> Int -> VMUpdater ()
writeMemoryOrRegister n val
  | n < 32768 = writeMemory n val
  | otherwise = writeRegister (n-32768) val

-- get argument n for current operation
readArg :: Int -> VMUpdater Int
readArg n = do
  state <- get
  let ip = vmIP state
  readMemory (ip+1+n)

-- get argument n for current operation, converting to value in register if needed
argValue :: Int -> VMUpdater Int
argValue n = do
  arg <- readArg n
  if arg >= 32768 then readRegister (arg-32768) else return arg

incIP :: Int -> VMUpdater ()
incIP n = do
  state <- get
  let ip = vmIP state
  put $ state { vmIP = ip + n }

decodeOpCode :: Int -> String
decodeOpCode 0  = "HLT"
decodeOpCode 6  = "JMP"
decodeOpCode 9  = "ADD"
decodeOpCode 19 = "OUT"
decodeOpCode 21 = "NOP"
decodeOpCode n  = error $ "Unknown opcode " <> show n

stepVM :: VMUpdater ()
stepVM = do
  state <- get
  let ip = vmIP state
  op    <- readMemory ip
  argA  <- readArg 0
  argB  <- readArg 1
  argC  <- readArg 2
  valA  <- argValue 0
  valB  <- argValue 1
  valC  <- argValue 2

  case decodeOpCode op of
    "HLT" -> halt
    "JMP" -> modify (\s -> s { vmIP = valA })
    "ADD" -> writeMemoryOrRegister argA ((valB + valC) `mod` 32768) >> incIP 4
    "OUT" -> liftIO (putChar $ chr argA) >> incIP 2
    "NOP" -> incIP 1
    _     -> error $ "Unknown opcode " <> show op
  where halt  = modify (\s -> s { vmHalt = True })

runVM :: VMState -> IO VMState
runVM state = do
  state' <- execStateT stepVM state
  if vmHalt state' then return state' else runVM state'

dumpRegisters :: VMState -> IO ()
dumpRegisters state = do
  putStrLn $ "IP = " <> show (vmIP state)
  putStrLn $ "R0 = " <> show (head $ vmRegisters state)
  putStrLn $ "R1 = " <> show (vmRegisters state !! 1)
  putStrLn $ "R2 = " <> show (vmRegisters state !! 2)
  putStrLn $ "R3 = " <> show (vmRegisters state !! 3)
  putStrLn $ "R4 = " <> show (vmRegisters state !! 4)
  putStrLn $ "R5 = " <> show (vmRegisters state !! 5)
  putStrLn $ "R6 = " <> show (vmRegisters state !! 6)
  putStrLn $ "R7 = " <> show (vmRegisters state !! 7)
  putStrLn $ "HALT = " <> show (vmHalt state)

