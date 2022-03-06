module BrickRunner
  ( brickRun
  ) where

import VMState
import Decoder
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Data.Maybe (maybeToList)
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as Bs

data UIState = UIState
  { vmState :: VMState
  , output  :: String
  } deriving (Eq, Show)

registersBrick :: UIState -> Widget n
registersBrick state = vBox [ reg 0, reg 1, reg 2, reg 3, reg 4, reg 5, reg 6, reg 7, regip, reghlt ]
  where vmstate = vmState state
        reg r   = str $ "R" <> show r <> "   = " <> hex ( vmRegisters vmstate !! r)
        regip   = str $ "IP   = " <> hex ( vmIP vmstate )
        reghlt  = str $ "HALT = " <> show ( vmHalt vmstate )

opCodeRow :: UIState -> (Bool, Int) -> Widget n
opCodeRow state (hi, i) = padRight (Pad (27-(length line + 2))) $ str line
  where vmstate   = vmState state
        op        = vmMemory vmstate !! i
        argA      = vmMemory vmstate !! (i + 1)
        argB      = vmMemory vmstate !! (i + 2)
        argC      = vmMemory vmstate !! (i + 3)
        leftpad   = padLeft (Pad 1)
        addr      = hex i
        hi'       = if hi then ">" else " "
        opstr     = decodeOpCode op <> replicate (4 - length (decodeOpCode op)) ' '
        show' n   = if n >= 32768 then "R" <> show (n-32768) else hex n
        numargs   = numArgs op
        argStrs   = map show' $ take numargs [ argA, argB, argC ]
        line      = unwords $ addr : hi' : opstr : argStrs

opCodeBrick :: UIState -> Widget n
opCodeBrick state = vBox $ take 10 $ zipWith (curry (opCodeRow state)) (True : repeat False) ips
  where vmstate   = vmState state
        ip        = vmIP vmstate
        opcode i  = vmMemory vmstate !! i
        ips       = iterate (\i -> i + numArgs (opcode i) + 1) ip

stackBrick :: UIState -> Widget n
stackBrick state = vBox $ zipWith (curry (str . show')) [0..] $ vmStack $ vmState state
  where show' (i, v)  = hex i <> " : " <> hex v

drawUI :: UIState -> [ Widget n ]
drawUI state = [ joinBorders ( opcodes <+> output' <+> (registers <=> stack) ) ]
  where output'   = B.border $ padRight Max $ str $ output state
        registers = B.border $ registersBrick state
        opcodes   = B.border $ opCodeBrick state
        stack     = B.border $ stackBrick state

eventHandler :: UIState -> BrickEvent n e -> EventM n (Next UIState)
eventHandler state (VtyEvent e) = case e of
  V.EvKey (V.KChar 'q') []  -> halt state
  V.EvKey (V.KChar 's') []  -> continue state'
  _                         -> continue state
  where (out', vmstate')  = stepVM $ vmState state
        state'            = UIState { vmState = vmstate', output = output state <> maybeToList out' }
eventHandler state _ = continue state

attrMap :: UIState -> A.AttrMap
attrMap s = A.attrMap V.defAttr []

app :: App UIState e ()
app = App { appDraw         = drawUI
          , appChooseCursor = \_ _ -> Nothing
          , appHandleEvent  = eventHandler
          , appStartEvent   = return
          , appAttrMap      = attrMap
          }

brickRun :: VMState -> IO VMState
brickRun state = do
  uistate' <- defaultMain app $ UIState { vmState = state, output = "" }
  return $ vmState uistate'
