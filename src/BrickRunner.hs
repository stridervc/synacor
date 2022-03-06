module BrickRunner
  ( brickRun
  ) where

import VMState
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V

type UIState = VMState

drawUI :: UIState -> [ Widget n ]
drawUI state = [ str "Hello" ]

eventHandler :: UIState -> BrickEvent n e -> EventM n (Next UIState)
eventHandler state (VtyEvent e) = case e of
  V.EvKey (V.KChar 'q') []  -> halt state
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
brickRun = defaultMain app
