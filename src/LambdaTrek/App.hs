{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek.App where

import Control.Monad.State
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
import LambdaTrek.Simulation.Ship
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
  VtyEvent (V.EvKey (V.KFun 1) _) -> do
    s <- gets formState
    modify . updateFormState . (\s' -> s' & gameStateScreen .~ QuadrantScreen) $ s
  VtyEvent (V.EvKey (V.KFun 2) _) -> do
    s <- gets formState
    modify . updateFormState . (\s' -> s' & gameStateScreen .~ SectorScreen) $ s
  VtyEvent (V.EvKey (V.KFun 3) _) -> do
    s <- gets formState
    modify . updateFormState . (\s' -> s' & gameStateScreen .~ SettingsScreen) $ s
  VtyEvent (V.EvKey V.KEnter []) -> do
    s <- gets formState
    if s^.gameStateGameOver
      then pure ()
      else runCommand s
  VtyEvent (V.EvKey V.KDown []) -> do
    let scrollHandle = viewportScroll SectorDialog
    vScrollBy scrollHandle 1
  VtyEvent (V.EvKey V.KUp []) -> do
    let scrollHandle = viewportScroll SectorDialog
    vScrollBy scrollHandle (-1)
  _ -> handleFormEvent ev

runCommand :: GameState -> EventM Name (Form GameState e Name) ()
runCommand s =
  case runCommandParser . T.unpack . T.strip. T.toUpper $ s^.gameStateCommandInput of
      Left commandError ->
        modify
        . updateFormState
        . (\s' -> (`execState` s') updateSimulation)
        $ s { _gameStateCommandError = Just $ renderCommandParseError commandError
            , _gameStateCommand = Nothing
            , _gameStateCommandInput = ""
            }
      Right command ->
        modify
        . updateFormState
        . checkForGameOver
        . (\s' -> (`execState` s') updateSimulation)
        $ s { _gameStateCommand = Just command
            , _gameStateCommandError = Nothing
            , _gameStateCommandInput = ""
            }

lambdaChooseCursor :: Form GameState e Name -> [CursorLocation Name] -> Maybe (CursorLocation Name)
lambdaChooseCursor _ _ = Nothing

lambdaStartEvent :: EventM Name s ()
lambdaStartEvent = pure ()

lambdaAttrMap :: Form GameState e Name -> AttrMap
lambdaAttrMap _ = attrMap defAttr
  [ (attrName "highlight-error", fg V.red)
  , (attrName "highlight-helm", fg V.yellow)
  ]

checkForGameOver :: GameState -> GameState
checkForGameOver gameState =
  let ship = gameState^.gameStateShip
  in if ship^.energy <= 0 || ship^.hull <= 0
     then gameState & (gameStateGameOver .~ True)
                    . (gameStateScreen .~ GameOverScreen)
     else gameState
