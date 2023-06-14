module LambdaTrek.App where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Graphics.Vty.Attributes (defAttr)
import qualified Graphics.Vty as V
import LambdaTrek.Render
import LambdaTrek.State

lambdaTrekApp :: App GameState e ()
lambdaTrekApp
  = App
  { appDraw         = lambdaRender
  , appChooseCursor = lambdaChooseCursor
  , appHandleEvent  = lambdaHandleEvent
  , appStartEvent   = lambdaStartEvent
  , appAttrMap      = lambdaAttrMap
  }

lambdaHandleEvent :: BrickEvent () e -> EventM () GameState ()
lambdaHandleEvent (VtyEvent ev) = case ev of
  V.EvResize _ _ -> pure ()
  V.EvKey V.KEsc [] -> halt
  _ -> pure ()
lambdaHandleEvent _ = pure ()

lambdaChooseCursor :: GameState -> [CursorLocation n] -> Maybe (CursorLocation n)
lambdaChooseCursor _ _ = Nothing

lambdaStartEvent :: EventM n s ()
lambdaStartEvent = pure ()

lambdaAttrMap :: GameState -> AttrMap
lambdaAttrMap _ = attrMap defAttr []
