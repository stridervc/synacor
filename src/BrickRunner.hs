{-# Language OverloadedStrings #-}

module BrickRunner
  ( brickRun
  ) where

import VMState
import Decoder
import Brick.Main
import Brick.Types
import Brick.BChan
import Brick.Util (on)
import Brick.Widgets.Core
import Brick.Widgets.Table
import Brick.Widgets.Center
import Data.Char (chr)
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
data UIMode   = ModeMain | ModeMemory | ModeMemDiff deriving (Eq, Show)

data UIState = UIState
  { vmState   :: VMState
  , running   :: Bool
  , uiMode    :: UIMode
  , memDPage  :: Int      -- ^ Mem dump page
  , memDCols  :: Int      -- ^ Mem dump columns
  , memDRows  :: Int      -- ^ Mem dump rows
  , memDSave  :: [Int]    -- ^ Mem dump save, 32768 Integers
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
hotkeysBrick = str $ intercalate " | " [ "F2 - Save", "F3 - Load", "F4 - Run", "F5 - Step", "F6 - Memory", "F7 - Mem Save", "F8 - Mem Diff", "F10 - Quit" ]

inputBrick :: UIState -> Widget n
inputBrick = str . vmInBuffer . vmState

memoryBrick :: UIState -> Widget n
memoryBrick state = centerLayer $ B.borderWithLabel (str "Memory") content
  where cols      = memDCols state
        rows      = memDRows state
        page      = memDPage state
        offs      = page * cols * rows
        content   = vBox $ header : header2 : take rows (map memrow [offs,offs+cols..32751])
        header    = str $ "     | " <> unwords (map hex (take cols [0..])) <> " |"
        header2   = str $ "-----+" <> concat (replicate (cols*5) "-") <> "-+-" <> concat (replicate cols "-")
        vmstate   = vmState state
        readmem i = vmMemory vmstate !! i
        memrow r  = let m = take cols (map (readmem . (+r)) [0..]) in
                      str $ hex r <> " | " <> unwords (map hex m) <> " | " <> concatMap ascii m
        ascii i   | i <= 32   = "."
                  | i >= 127  = "."
                  | otherwise = [chr i]

memoryDiffBrick :: UIState -> Widget n
memoryDiffBrick state = centerLayer $ B.borderWithLabel (str "Memory Diff") content
  where vms                   = vmState state
        mem                   = vmMemory vms
        saved                 = memDSave state
        diff                  = take 32 [ ( i, saved !! i, mem !! i) | i <- [0..32767], saved !! i /= mem !! i ]
        diffrow (i, old, new) = str $ hex i <> " | " <> hex old <> " -> " <> hex new
        content               = vBox $ map diffrow diff

drawUI :: UIState -> [ Widget UINames ]
drawUI state
  | memdiffui = [ memoryDiffBrick state, withAttr "faded" mainlayout ]
  | memui     = [ memoryBrick state, withAttr "faded" mainlayout ]
  | otherwise = [ mainlayout ]
  where memui       = uiMode state == ModeMemory
        memdiffui   = uiMode state == ModeMemDiff
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
eventHandler state (VtyEvent e)
  | memdiffui = case e of
    V.EvKey V.KEsc []       -> continue state { uiMode = ModeMain }
    _                       -> continue state
  | memui     = case e of
    V.EvKey V.KEsc []       -> continue state { uiMode = ModeMain }
    V.EvKey V.KPageDown []  -> continue state { memDPage = if page*cols*rows < 32768-cols*rows then page + 1 else page }
    V.EvKey V.KPageUp []    -> continue state { memDPage = if page > 0 then page - 1 else page }
    _                       -> continue state
  | otherwise = case e of
    V.EvKey (V.KFun 10) []  -> halt state
    V.EvKey (V.KFun 8) []   -> continue state { uiMode = ModeMemDiff, running = False }
    V.EvKey (V.KFun 7) []   -> continue state { memDSave = vmMemory vms }
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
          memui     = uiMode state == ModeMemory
          memdiffui = uiMode state == ModeMemDiff
          page      = memDPage state
          cols      = memDCols state
          rows      = memDRows state
eventHandler state _ = continue state

attrMap :: UIState -> A.AttrMap
attrMap s = A.attrMap V.defAttr [ ("faded", V.red `on` V.black) ]

app :: App UIState UIEvent UINames
app = App { appDraw         = drawUI
          , appChooseCursor = \_ _ -> Nothing
          , appHandleEvent  = eventHandler
          , appStartEvent   = return
          , appAttrMap      = attrMap
          }

uiState :: VMState -> UIState
uiState state = UIState { vmState   = state
                        , running   = False
                        , uiMode    = ModeMain
                        , memDPage  = 0
                        , memDCols  = 16
                        , memDRows  = 32
                        , memDSave  = replicate 32768 0
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
