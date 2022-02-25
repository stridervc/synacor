module VM
  ( VMState (..)
  , newVMState
  ) where

import Control.Monad.State

type Register = Int

data VMState = VMState
  { vmMemory    :: [Int]      -- ^ 32768 Integers
  , vmRegisters :: [Register] -- ^ 8 Integers
  , vmStack     :: [Int]      -- ^ Unbounded stack
  , vmIP        :: Int        -- ^ Instruction Pointer
  } deriving (Eq, Show)

newVMState :: VMState
newVMState = VMState
  { vmMemory    = replicate 32768 0
  , vmRegisters = replicate 8 0
  , vmStack     = []
  , vmIP        = 0
  }
