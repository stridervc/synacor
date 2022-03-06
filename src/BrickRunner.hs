module BrickRunner
  ( brickRun
  ) where

import VMState
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Data.Maybe (maybeToList)
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V

data UIState = UIState
  { vmState :: VMState
  , output  :: String
  } deriving (Eq, Show)

registersBrick :: UIState -> Widget n
registersBrick state = vBox [ reg 0, reg 1, reg 2, reg 3, reg 4, reg 5, reg 6, reg 7, regip, reghlt ]
  where vmstate = vmState state
        reg r   = str $ "R" <> show r <> "   = " <> show ( vmRegisters vmstate !! r)
        regip   = str $ "IP   = " <> show ( vmIP vmstate )
        reghlt  = str $ "HALT = " <> show ( vmHalt vmstate )

drawUI :: UIState -> [ Widget n ]
drawUI state = [ output' <+> registersBrick state]
  where output' = padRight Max $ str $ output state

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
