{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek.App where

import Brick.AttrMap
import Brick.Forms
import Brick.Main
import Brick.Types
import Brick.Util
import qualified Data.Text as T
import Graphics.Vty.Attributes (defAttr)
import qualified Graphics.Vty as V
import LambdaTrek.Command.Parse
import LambdaTrek.Render
import LambdaTrek.Simulation
import LambdaTrek.State
import LambdaTrek.UI
import Lens.Micro

lambdaTrekApp :: App (Form GameState e Name) e Name
lambdaTrekApp
  = App
  { appDraw         = lambdaRender
  , appChooseCursor = lambdaChooseCursor
  , appHandleEvent  = lambdaHandleEvent
  , appStartEvent   = lambdaStartEvent
  , appAttrMap      = lambdaAttrMap
  }

lambdaHandleEvent :: BrickEvent Name e -> EventM Name (Form GameState e Name) ()
lambdaHandleEvent ev = case ev of
  VtyEvent (V.EvResize _ _) -> pure ()
  VtyEvent (V.EvKey V.KEsc []) -> halt
  VtyEvent (V.EvKey V.KEnter []) -> do
    s <- gets formState
    case runCommandParser . T.unpack $ s^.gameStateCommandInput of
      Left commandError ->
        modify
        . updateFormState
        . updateSimulation
        $ s { _gameStateCommandError = Just $ renderCommandParseError commandError
            , _gameStateCommand = Nothing
            , _gameStateCommandInput = ""
            }
      Right command ->
        modify
        . updateFormState
        . updateSimulation
        $ s { _gameStateCommand = Just command
            , _gameStateCommandError = Nothing
            , _gameStateCommandInput = ""
            }
  _ -> handleFormEvent ev

lambdaChooseCursor :: Form GameState e Name -> [CursorLocation Name] -> Maybe (CursorLocation Name)
lambdaChooseCursor _ _ = Nothing

lambdaStartEvent :: EventM Name s ()
lambdaStartEvent = pure ()

lambdaAttrMap :: Form GameState e Name -> AttrMap
lambdaAttrMap _ = attrMap defAttr [(attrName "highlight-error", fg V.red)]
