module BrickRunner
  ( brickRun
  ) where

import VMState
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V

data UIState = UIState
  { vmState :: VMState
  , output  :: String
  } deriving (Eq, Show)

drawUI :: UIState -> [ Widget n ]
drawUI state = [ str $ output state ]

eventHandler :: UIState -> BrickEvent n e -> EventM n (Next UIState)
eventHandler state (VtyEvent e) = case e of
  V.EvKey (V.KChar 'q') []  -> halt state
  V.EvKey (V.KChar 's') []  -> do
                                let (out', vmstate') = stepVM $ vmState state
                                case out' of
                                  Just c  -> continue $ state { vmState = vmstate', output = output state <> [c] }
                                  Nothing -> continue $ state { vmState = vmstate' }
  _                         -> continue state
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
