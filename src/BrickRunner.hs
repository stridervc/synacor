module BrickRunner
  ( brickRun
  ) where

import VMState

brickRun :: VMState -> IO VMState
brickRun = runVM
