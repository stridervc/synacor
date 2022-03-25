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
data UIMode   = ModeCommand | ModeInteractive | ModeMemory | ModeMemDiff deriving (Eq, Show)

data UIState = UIState
  { vmState     :: VMState
  , running     :: Bool
  , uiMode      :: UIMode
  , memDPage    :: Int      -- ^ Mem dump page
  , memDCols    :: Int      -- ^ Mem dump columns
  , memDRows    :: Int      -- ^ Mem dump rows
  , memDSave    :: [Int]    -- ^ Mem dump save, 32768 Integers
  , command     :: String
  , lastCommand :: String
  } deriving (Eq, Show)

registersBrick :: UIState -> Widget n
registersBrick state = vBox [ reg 0, reg 1, reg 2, reg 3, reg 4, reg 5, reg 6, reg 7, regip, reghlt ]
  where vmstate = vmState state
        reg r   = str $ "R" <> show r <> "   = " <> hex ( vmRegisters vmstate !! r)
        regip   = str $ "IP   = " <> hex ( vmIP vmstate )
        reghlt  = str $ "HALT = " <> show ( vmHalt vmstate )

opCodeRow :: UIState -> (Bool, Int) -> Widget n
opCodeRow state (hi, i) = withAttr attr $ padRight (Pad (27-(length line + 2))) $ str line
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
        hasbp     = elem (BPIP i) $ vmBreakpoints $ vmState state
        attr      = if hasbp then "onred" else "normal"

opCodeBrick :: UIState -> Widget n
opCodeBrick state = vBox $ take 10 $ zipWith (curry (opCodeRow state)) (True : repeat False) ips
  where vmstate   = vmState state
        ip        = vmIP vmstate
        opcode i  = vmMemory vmstate !! i
        ips       = iterate (\i -> i + numArgs (opcode i) + 1) ip

stackBrick :: UIState -> Widget n
stackBrick state = vBox $ zipWith (curry (str . show')) [0..] $ take 20 $ vmStack $ vmState state
  where show' (i, v)  = hex i <> " : " <> hex v

memoryWatchBrick :: UIState -> Widget n
memoryWatchBrick state = vBox $ map show' watch
  where show' (n,a) = str $ n <> " : " <> a <> " : " <> hex (vmMemory (vmState state) !! fromHex a)
        watch       = [ ("room", "0aac")
                      , ("room", "0aad")
                      ]

hotkeysBrick :: Widget n
hotkeysBrick = str $ intercalate " | " [ "F2 - Save", "F3 - Load", "F4 - Run", "F5 - Step", "F6 - Memory", "F7 - Mem Save", "F8 - Mem Diff", "F9 - Step Over", "F10 - Quit" ]

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

commandPromptBrick :: UIState -> Widget n
commandPromptBrick state
  | cmd == "" = hBox $ map str [ "[", lastcmd, "]", " > " ]
  | otherwise = hBox $ map str [ "> ", cmd ]
  where cmd     = command state
        lastcmd = lastCommand state

drawUI :: UIState -> [ Widget UINames ]
drawUI state
  | mode == ModeMemDiff     = [ memoryDiffBrick state, withAttr "faded" mainlayout ]
  | mode == ModeMemory      = [ memoryBrick state, withAttr "faded" mainlayout ]
  | mode == ModeInteractive = [ mainlayout ]
  | otherwise               = [ cmdlayout ]
  where mode        = uiMode state
        output'     = B.border $ viewport OutputView Vertical $ padRight Max $ strWrap $ vmOutput $ vmState state
        registers   = B.border $ registersBrick state
        opcodes     = B.border $ opCodeBrick state
        stack       = B.border $ stackBrick state
        inbuff      = B.border $ inputBrick state
        hotkeys     = B.border hotkeysBrick
        prompt      = B.border $ commandPromptBrick state
        memwatch    = B.border $ memoryWatchBrick state
        mainlayout  = joinBorders ( ((opcodes <=> memwatch) <+> output' <+> (registers <=> stack) ) <=> hotkeys)
        cmdlayout   = joinBorders ( ((opcodes <=> memwatch) <+> output' <+> (registers <=> stack) ) <=> prompt <=> hotkeys)

writeMem :: VMState -> Int -> Int -> VMState
writeMem vms addr val = vms { vmMemory = mem' }
  where mem   = vmMemory vms
        mem'  = take addr mem <> [val] <> drop (addr+1) mem

setRoom :: VMState -> Int -> VMState
setRoom vms room = vms'
  where mem   = vmMemory vms
        vms'  = writeMem (writeMem vms (fromHex "0aac") room) (fromHex "0aad") room

execCommand :: UIState -> UIState
execCommand state = case cmd of
  ""      -> if lastall == "" then state else execCommand $ state { command = lastall }
  "i"     -> state' { uiMode = ModeInteractive }
  "step"  -> state' { vmState = stepVM vms, running = False }
  "run"   -> state' { running = not $ running state }
  "runi"  -> state' { running = True, uiMode = ModeInteractive }
  "stop"  -> state' { running = False }
  "over"  -> state' { vmState = addBreakpoint vms (BPIP nextip), running = True }
  "bp"    -> state' { vmState = addBreakpoint vms (BPIP fromhex) }
  "cbp"   -> state' { vmState = delBreakpoint vms (BPIP fromhex) }
  "room"  -> state' { vmState = setRoom vms fromhex }
  _       -> invcmd
  where cmd     = if null (command state) then "" else head $ words $ command state
        args    = tail $ words $ command state
        invcmd  = state { command = "" }
        state'  = invcmd { lastCommand = "step" }
        vms     = vmState state
        fromhex = fromHex $ head args
        lastall = lastCommand state
        op      = vmMemory vms !! ip
        ip      = vmIP vms
        nextip  = ip + 1 + numArgs op

eventHandler :: UIState -> BrickEvent UINames UIEvent -> EventM UINames (Next UIState)
eventHandler state (AppEvent StepEvent)
  | hitbp     = continue state { running = False }
  | running'  = scroll >> continue state'
  | otherwise = continue state
  where vmstate'  = stepVM $ vmState state
        state'    = state { vmState = vmstate' }
        running'  = running state
        scroll    = vScrollToEnd (viewportScroll OutputView)
        hitbp     = elem (BPIP (vmIP (vmState state))) $ vmBreakpoints $ vmState state
eventHandler state (VtyEvent e)
  | mode == ModeMemDiff     = case e of
    V.EvKey V.KEsc []       -> continue state { uiMode = ModeCommand }
    _                       -> continue state
  | mode == ModeMemory      = case e of
    V.EvKey V.KEsc []       -> continue state { uiMode = ModeCommand }
    V.EvKey V.KPageDown []  -> continue state { memDPage = if page*cols*rows < 32768-cols*rows then page + 1 else page }
    V.EvKey V.KPageUp []    -> continue state { memDPage = if page > 0 then page - 1 else page }
    _                       -> continue state
  | mode == ModeInteractive = case e of
    V.EvKey (V.KFun 10) []  -> halt state
    V.EvKey (V.KFun 9) []   -> continue state { vmState = stepOverVM vms }
    V.EvKey (V.KFun 8) []   -> continue state { uiMode = ModeMemDiff, running = False }
    V.EvKey (V.KFun 7) []   -> continue state { memDSave = vmMemory vms }
    V.EvKey (V.KFun 6) []   -> continue state { uiMode = ModeMemory, running = False }
    V.EvKey (V.KFun 5) []   -> continue state'
    V.EvKey (V.KFun 4) []   -> continue state { running = not $ running state }
    V.EvKey (V.KFun 2) []   -> liftIO (saveVM (vmState state) "vm.state") >> continue state
    V.EvKey (V.KFun 3) []   -> do
                                vms' <- liftIO (loadVM "vm.state")
                                continue (state { vmState = vms' })
    V.EvKey (V.KChar c) []  -> continue state { vmState = vms { vmInBuffer = inb <> [c] } }
    V.EvKey V.KEnter []     -> continue state { vmState = vms { vmInBuffer = inb <> ['\n'] } }
    V.EvKey V.KEsc []       -> scroll >> continue state { uiMode = ModeCommand }
    _                       -> continue state
  | mode == ModeCommand     = case e of
    V.EvKey (V.KFun 10) []  -> halt state
    V.EvKey (V.KFun 9) []   -> continue state { vmState = stepOverVM vms }
    V.EvKey (V.KFun 8) []   -> continue state { uiMode = ModeMemDiff, running = False }
    V.EvKey (V.KFun 7) []   -> continue state { memDSave = vmMemory vms }
    V.EvKey (V.KFun 6) []   -> continue state { uiMode = ModeMemory, running = False }
    V.EvKey (V.KFun 5) []   -> continue state'
    V.EvKey (V.KFun 4) []   -> continue state { running = not $ running state }
    V.EvKey (V.KFun 2) []   -> liftIO (saveVM (vmState state) "vm.state") >> continue state
    V.EvKey (V.KFun 3) []   -> do
                                vms' <- liftIO (loadVM "vm.state")
                                continue $ state { vmState = vms' }
    V.EvKey (V.KChar c) []  -> continue state { command = cmd <> [c] }
    V.EvKey V.KBS []        -> continue state { command = if cmd == "" then "" else init cmd }
    V.EvKey V.KEnter []     -> scroll >> continue (execCommand state)
    _                       -> continue state
    where vmstate'  = stepVM $ vmState state
          state'    = state { vmState = vmstate' }
          vms       = vmState state
          inb       = vmInBuffer vms
          mode      = uiMode state
          page      = memDPage state
          cols      = memDCols state
          rows      = memDRows state
          cmd       = command state
          scroll    = vScrollToEnd (viewportScroll OutputView)
eventHandler state _ = continue state

attrMap :: UIState -> A.AttrMap
attrMap s = A.attrMap V.defAttr [ ("faded", V.red `on` V.black)
                                , ("onred", V.white `on` V.red)
                                , ("normal", V.white `on` V.black)
                                ]

app :: App UIState UIEvent UINames
app = App { appDraw         = drawUI
          , appChooseCursor = \_ _ -> Nothing
          , appHandleEvent  = eventHandler
          , appStartEvent   = return
          , appAttrMap      = attrMap
          }

uiState :: VMState -> UIState
uiState state = UIState
  { vmState     = state
  , running     = False
  , uiMode      = ModeCommand
  , memDPage    = 0
  , memDCols    = 16
  , memDRows    = 32
  , memDSave    = replicate 32768 0
  , command     = ""
  , lastCommand = "step"
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
