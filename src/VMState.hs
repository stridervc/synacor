{-# Language FlexibleContexts #-}

module VMState
  ( VMState (..)
  , newVMState
  , newVMFromInts
  , newVMFromFile
  , stepVM
  , runVM
  , saveVM
  , loadVM
  ) where

import Decoder
import Data.Bits
import Data.Char (chr, ord)
import Control.Monad.State
import Control.Monad.Extra (whenJust)
import System.IO (openBinaryFile, hGetContents, IOMode(..))

type Register   = Int
type Operator   = Int

type VMUpdater = State VMState

data VMState = VMState
  { vmMemory    :: [Int]      -- ^ 32768 Integers
  , vmRegisters :: [Register] -- ^ 8 Integers
  , vmStack     :: [Int]      -- ^ Unbounded stack
  , vmIP        :: Int        -- ^ Instruction Pointer
  , vmHalt      :: Bool       -- ^ Catch fire?
  , vmOutput    :: String     -- ^ VM's output
  } deriving (Eq, Show, Read)

newVMState :: VMState
newVMState = VMState
  { vmMemory    = replicate 32768 0
  , vmRegisters = replicate 8 0
  , vmStack     = []
  , vmIP        = 0
  , vmHalt      = False
  , vmOutput    = ""
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
  h <- openBinaryFile f ReadMode
  contents <- hGetContents h
  return $ newVMFromInts $ charPairsToInts contents

readMemory :: Int -> VMUpdater Int
readMemory n = gets (\state -> vmMemory state !! n)

writeMemory :: Int -> Int -> VMUpdater ()
writeMemory n val = do
  mem <- gets vmMemory
  modify (\state -> state { vmMemory = take n mem <> [val] <> drop (n+1) mem })

readRegister :: Int -> VMUpdater Int
readRegister n = gets (\state -> vmRegisters state !! n)

writeRegister :: Int -> Int -> VMUpdater ()
writeRegister n val = do
  regs <- gets vmRegisters
  modify (\state -> state { vmRegisters = take n regs <> [val] <> drop (n+1) regs })

writeMemoryOrRegister :: Int -> Int -> VMUpdater ()
writeMemoryOrRegister n val
  | n < 32768 = writeMemory n val
  | otherwise = writeRegister (n-32768) val

-- get argument n for current operation
readArg :: Int -> VMUpdater Int
readArg n = do
  ip <- readIP
  readMemory (ip+1+n)

-- get argument n for current operation, converting to value in register if needed
argValue :: Int -> VMUpdater Int
argValue n = do
  arg <- readArg n
  if arg >= 32768 then readRegister (arg-32768) else return arg

readIP :: VMUpdater Int
readIP = gets vmIP

incIP :: Int -> VMUpdater ()
incIP n = do
  ip <- readIP
  modify (\s -> s { vmIP = ip + n })

pushStack :: Int -> VMUpdater ()
pushStack v = do
  stack <- gets vmStack
  modify (\s -> s { vmStack = v : stack })

popStack :: VMUpdater Int
popStack = do
  stack <- gets vmStack
  modify (\s -> s { vmStack = tail stack })
  return $ head stack

stepVM' :: VMUpdater ()
stepVM' = do
  ip    <- readIP
  op    <- readMemory ip
  argA  <- readArg 0
  argB  <- readArg 1
  argC  <- readArg 2
  valA  <- argValue 0
  valB  <- argValue 1
  valC  <- argValue 2

  case decodeOpCode op of
    "HALT"  -> halt
    "SET"   -> writeMemoryOrRegister argA valB >> adv op
    "PUSH"  -> pushStack valA >> adv op
    "POP"   -> popStack >>= writeMemoryOrRegister argA >> adv op
    "EQ"    -> writeMemoryOrRegister argA (if valB == valC then 1 else 0) >> adv op
    "GT"    -> writeMemoryOrRegister argA (if valB > valC then 1 else 0) >> adv op
    "JMP"   -> jmp valA
    "ADD"   -> writeMemoryOrRegister argA ((valB + valC) `mod` 32768) >> adv op
    "MUL"   -> writeMemoryOrRegister argA ((valB * valC) `mod` 32768) >> adv op
    "MOD"   -> writeMemoryOrRegister argA (valB `mod` valC) >> adv op
    "AND"   -> writeMemoryOrRegister argA (valB .&. valC) >> adv op
    "OR"    -> writeMemoryOrRegister argA (valB .|. valC) >> adv op
    "NOT"   -> writeMemoryOrRegister argA (complement valB .&. 32767) >> adv op
    "RMEM"  -> readMemory valB >>= writeMemoryOrRegister argA >> adv op
    "WMEM"  -> writeMemoryOrRegister valA valB  >> adv op
    "JT"    -> (if valA /= 0 then jmp valB else adv op)
    "JF"    -> (if valA == 0 then jmp valB else adv op)
    "OUT"   -> adv op >> modify (\s -> s { vmOutput = vmOutput s <> [chr valA] })
    "NOP"   -> adv op
    "CALL"  -> pushStack (ip + numArgs op + 1) >> jmp valA
    "RET"   -> popStack >>= jmp
    _       -> halt
  where halt  = modify (\s -> s { vmHalt = True })
        jmp i = modify (\s -> s { vmIP = i })
        adv n = incIP $ numArgs n + 1

stepVM :: VMState -> VMState
stepVM = execState stepVM'

runVM :: VMState -> VMState
runVM state
  | vmHalt state' = state'
  | otherwise     = runVM state'
  where state' = stepVM state

saveVM :: VMState -> FilePath -> IO ()
saveVM state file = writeFile file $ show state

loadVM :: FilePath -> IO VMState
loadVM file = do
  contents <- readFile file
  return $ read contents
