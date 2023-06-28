{-# LANGUAGE LambdaCase #-}

module LambdaTrek.Simulation where

import LambdaTrek.Command
import LambdaTrek.Simulation.Ship
import LambdaTrek.State
import Lens.Micro

updateSimulation :: GameState -> GameState
updateSimulation gameState =
  case gameState^.gameStateCommand of
    Just cmd -> handleCommand gameState cmd & gameStateCommand .~ Nothing
    _ -> gameState

handleCommand :: GameState -> Command -> GameState
handleCommand gameState = \case
  EngineMove x y -> gameState & gameStateShip .~ Ship x y
  JumpMove _ -> gameState
