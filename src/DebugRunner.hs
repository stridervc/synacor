module DebugRunner
  ( debugRun
  ) where

import VMState
import System.IO (hFlush, stdout)
import System.Console.ANSI
import Control.Monad (when)
import Control.Monad.Extra (whenJust)

prompt :: IO (Int, Int)
prompt = do
  Just (y, x)   <- getCursorPosition
  Just (sy, sx) <- getTerminalSize
  when (y + 1 == sy) $ scrollPageUp 1
  setCursorPosition (sy-1) 0
  putStr "> "
  hFlush stdout
  if y + 1 == sy then return (y-1, x) else return (y, x)

debugRun :: VMState -> IO VMState
debugRun state = do
  (y, x) <- prompt
  input <- getLine
  scrollPageDown 1
  clearLine
  setCursorPosition y x

  case input of
    "quit"  -> return state
    "run"   -> runVM state
    ""      -> do
      let (out', state') = stepVM state
      whenJust out' putChar
      debugRun state'
    _       -> debugRun state
