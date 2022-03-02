module DebugRunner
  ( debugRun
  ) where

import VMState
import System.IO (hFlush, stdout)
import System.Console.ANSI
import Control.Monad (when)

debugRun :: VMState -> IO VMState
debugRun state = do
  Just (y,x) <- getCursorPosition
  putStrLn ""
  putStr "> "
  hFlush stdout
  input <- getLine
  cursorUpLine 1
  clearLine
  cursorUpLine 1
  when (x > 0) $ cursorForward x

  case input of
    "quit"  -> return state
    ""      -> do
      state' <- stepVM state
      debugRun state'
    _       -> putStrLn input >> debugRun state
