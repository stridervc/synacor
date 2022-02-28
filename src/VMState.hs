module VMState
  ( VMState (..)
  , newVMState
  , newVMFromInts
  , stepVM
  , runVM
  ) where

import Data.Char (chr)
import Control.Monad.State

type Register   = Int
type Operator   = Int

type VMUpdater  = StateT VMState IO

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

stepVM :: VMUpdater ()
stepVM = do
  state <- get
  let ip  = vmIP state
  op <- readMemory ip
  argA <- argValue (ip+1)
  argB <- argValue (ip+2)
  argC <- argValue (ip+3)

  case op of
    0   -> put $ incIP 1 state
    9   -> do
      writeMemoryOrRegister argA ((argB + argC) `mod` 32768)
      advanceIP 3
    19  -> do
      liftIO $ putChar $ chr argA
      put $ incIP 2 state
    21  -> put $ incIP 1 state
    _   -> undefined
  where incIP n state = state { vmIP = vmIP state + n }

runVM :: VMState -> IO ()
runVM state = do
  state' <- execStateT stepVM state
  if vmHalt state' then return () else runVM state'
