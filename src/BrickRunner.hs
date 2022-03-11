module BrickRunner
  ( brickRun
  ) where

import VMState
import Decoder
import Brick.Main
import Brick.Types
import Brick.BChan
import Brick.Widgets.Core
import Brick.Widgets.Table
import Brick.Widgets.Center
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
data UINames  = OutputView deriving (Eq, Show, Ord)
data UIMode   = ModeMain | ModeMemory deriving (Eq, Show)

data UIState = UIState
  { vmState :: VMState
  , running :: Bool
  , uiMode  :: UIMode
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
stackBrick state = vBox $ zipWith (curry (str . show')) [0..] $ take 20 $ vmStack $ vmState state
  where show' (i, v)  = hex i <> " : " <> hex v

hotkeysBrick :: Widget n
hotkeysBrick = str $ intercalate " | " [ "F2 - Save", "F3 - Load", "F4 - Run", "F5 - Step", "F6 - Memory", "F10 - Quit" ]

inputBrick :: UIState -> Widget n
inputBrick = str . vmInBuffer . vmState

memoryBrick :: UIState -> Widget n
memoryBrick state = centerLayer $ B.borderWithLabel (str "Memory") content
  where cols      = 16
        content   = vBox $ header : header2 : take 32 (map memrow [0,cols..32751])
        header    = str $ "     | " <> unwords (map hex (take cols [0..]))
        header2   = str $ "-----+" <> concat (replicate (cols*5) "-")
        vmstate   = vmState state
        readmem i = vmMemory vmstate !! i
        memrow r  = str $ hex r <> " | " <> unwords (take cols (map (hex . readmem . (+r)) [0..]))

drawUI :: UIState -> [ Widget UINames ]
drawUI state
  | memui     = [ memoryBrick state, mainlayout ]
  | otherwise = [ mainlayout ]
  where mainui      = uiMode state == ModeMain
        memui       = uiMode state == ModeMemory
        output'     = B.border $ viewport OutputView Vertical $ padRight Max $ strWrap $ vmOutput $ vmState state
        registers   = B.border $ registersBrick state
        opcodes     = B.border $ opCodeBrick state
        stack       = B.border $ stackBrick state
        inbuff      = B.border $ inputBrick state
        hotkeys     = B.border hotkeysBrick
        mainlayout  = joinBorders ( (opcodes <+> output' <+> (registers <=> stack) ) <=> hotkeys)

eventHandler :: UIState -> BrickEvent UINames UIEvent -> EventM UINames (Next UIState)
eventHandler state (AppEvent StepEvent)
  | running'  = scroll >> continue state'
  | otherwise = scroll >> continue state
  where vmstate'  = stepVM $ vmState state
        state'    = state { vmState = vmstate' }
        running'  = running state
        scroll    = vScrollToEnd (viewportScroll OutputView)
eventHandler state (VtyEvent e) = case e of
  V.EvKey (V.KFun 10) []  -> halt state
  V.EvKey (V.KFun 7) []   -> continue state { uiMode = ModeMain }
  V.EvKey (V.KFun 6) []   -> continue state { uiMode = ModeMemory, running = False }
  V.EvKey (V.KFun 5) []   -> continue state'
  V.EvKey (V.KFun 4) []   -> continue state { running = not $ running state }
  V.EvKey (V.KFun 2) []   -> liftIO (saveVM (vmState state) "vm.state") >> continue state
  V.EvKey (V.KFun 3) []   -> do
                              vms' <- liftIO (loadVM "vm.state")
                              continue $ state { vmState = vms' }
  V.EvKey (V.KChar c) []  -> continue state { vmState = vms { vmInBuffer = inb <> [c] } }
  V.EvKey V.KEnter []     -> continue state { vmState = vms { vmInBuffer = inb <> ['\n'] } }
  _                       -> continue state
  where vmstate'  = stepVM $ vmState state
        state'    = state { vmState = vmstate' }
        vms       = vmState state
        inb       = vmInBuffer vms
eventHandler state _ = continue state

attrMap :: UIState -> A.AttrMap
attrMap s = A.attrMap V.defAttr []

app :: App UIState UIEvent UINames
app = App { appDraw         = drawUI
          , appChooseCursor = \_ _ -> Nothing
          , appHandleEvent  = eventHandler
          , appStartEvent   = return
          , appAttrMap      = attrMap
          }

uiState :: VMState -> UIState
uiState state = UIState { vmState = state
                        , running = False
                        , uiMode  = ModeMain
                        }

brickRun :: VMState -> IO VMState
brickRun state = do
  chan <- newBChan 10
  void $ forkIO $ forever $ do
    writeBChan chan StepEvent
    threadDelay 1000

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  uistate' <- customMain initialVty buildVty (Just chan) app $ uiState state
  return $ vmState uistate'
