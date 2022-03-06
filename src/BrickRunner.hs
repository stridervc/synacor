module BrickRunner
  ( brickRun
  ) where

import VMState
import Decoder
import Brick.Main
import Brick.Types
import Brick.BChan
import Brick.Widgets.Core
import Data.Maybe (maybeToList)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.List (intercalate)
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as Bs

data UIEvent  = StepEvent deriving (Eq, Show)

data UIState = UIState
  { vmState :: VMState
  , running :: Bool
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

hotkeysBrick :: Widget n
hotkeysBrick = str $ intercalate " | " [ "F2 - Save", "F3 - Load", "F4 - Run", "F5 - Step", "F10 - Quit" ]

inputBrick :: UIState -> Widget n
inputBrick = str . vmInBuffer . vmState

drawUI :: UIState -> [ Widget n ]
drawUI state = [ joinBorders ( (opcodes <+> output' <+> (registers <=> stack) ) <=> inbuff <=> hotkeys) ]
  where output'   = B.border $ padRight Max $ strWrap $ vmOutput $ vmState state
        registers = B.border $ registersBrick state
        opcodes   = B.border $ opCodeBrick state
        stack     = B.border $ stackBrick state
        inbuff    = B.border $ inputBrick state
        hotkeys   = B.border hotkeysBrick

eventHandler :: UIState -> BrickEvent n UIEvent -> EventM n (Next UIState)
eventHandler state (AppEvent StepEvent)
  | running'  = continue state'
  | otherwise = continue state
  where vmstate'  = stepVM $ vmState state
        state'    = state { vmState = vmstate' }
        running'  = running state
eventHandler state (VtyEvent e) = case e of
  V.EvKey (V.KFun 10) []  -> halt state
  V.EvKey (V.KFun 5) []   -> continue state'
  V.EvKey (V.KFun 4) []   -> continue state { running = not $ running state }
  V.EvKey (V.KFun 2) []   -> liftIO (saveVM (vmState state) "vm.state") >> continue state
  V.EvKey (V.KFun 3) []   -> do
                              vms' <- liftIO (loadVM "vm.state")
                              continue $ state { vmState = vms' }
  V.EvKey (V.KChar c) []  -> continue $ state { vmState = vms { vmInBuffer = inb <> [c] } }
  V.EvKey V.KEnter []     -> continue $ state { vmState = vms { vmInBuffer = inb <> ['\n'] } }
  _                       -> continue state
  where vmstate'  = stepVM $ vmState state
        state'    = state { vmState = vmstate' }
        vms       = vmState state
        inb       = vmInBuffer vms
eventHandler state _ = continue state

attrMap :: UIState -> A.AttrMap
attrMap s = A.attrMap V.defAttr []

app :: App UIState UIEvent ()
app = App { appDraw         = drawUI
          , appChooseCursor = \_ _ -> Nothing
          , appHandleEvent  = eventHandler
          , appStartEvent   = return
          , appAttrMap      = attrMap
          }

brickRun :: VMState -> IO VMState
brickRun state = do
  chan <- newBChan 10
  void $ forkIO $ forever $ do
    writeBChan chan StepEvent
    threadDelay 1000

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  uistate' <- customMain initialVty buildVty (Just chan) app $ UIState { vmState = state, running = False }
  return $ vmState uistate'
