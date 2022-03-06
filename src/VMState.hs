{-# Language FlexibleContexts #-}

module VMState
  ( VMState (..)
  , newVMState
  , newVMFromInts
  , newVMFromFile
  , stepVM
  , runVM
  , dumpRegisters
  ) where

import Decoder
import Data.Char (chr, ord)
import Data.Bits (shiftL)
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

stepVM' :: VMUpdater (Maybe Char)
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
    "HALT"  -> halt >> return Nothing
    "SET"   -> writeMemoryOrRegister argA valB >> adv op >> return Nothing
    "PUSH"  -> pushStack valA >> adv op >> return Nothing
    "POP"   -> popStack >>= writeMemoryOrRegister argA >> adv op >> return Nothing
    "EQ"    -> writeMemoryOrRegister argA (if valB == valC then 1 else 0) >> adv op >> return Nothing
    "GT"    -> writeMemoryOrRegister argA (if valB > valC then 1 else 0) >> adv op >> return Nothing
    "JMP"   -> jmp valA >> return Nothing
    "ADD"   -> writeMemoryOrRegister argA ((valB + valC) `mod` 32768) >> adv op >> return Nothing
    "JT"    -> (if valA /= 0 then jmp valB else adv op) >> return Nothing
    "JF"    -> (if valA == 0 then jmp valB else adv op) >> return Nothing
    "OUT"   -> adv op >> return ( Just ( chr valA ) )
    "NOP"   -> adv op >> return Nothing
    _       -> halt >> return Nothing
  where halt  = modify (\s -> s { vmHalt = True })
        jmp i = modify (\s -> s { vmIP = i })
        adv n = incIP $ numArgs n + 1

stepVM :: VMState -> (Maybe Char, VMState)
stepVM = runState stepVM'

runVM :: VMState -> IO VMState
runVM state = do
  let (out', state') = stepVM state
  whenJust out' putChar
  if vmHalt state' then return state' else runVM state'

-- TODO this should be removed from here
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

