{-# LANGUAGE LambdaCase #-}

module LambdaTrek.Simulation where

import LambdaTrek.Command
import LambdaTrek.Simulation.Sector
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
  EngineMove x y -> handleEngineMove gameState x y
  JumpMove _ -> gameState

handleEngineMove :: GameState -> Int -> Int -> GameState
handleEngineMove gameState x y =
  let stars_ = gameState^.(gameStateSector . stars)
      enemies = gameState^.(gameStateSector . enemyShips)
  in if collidesWithStars stars_ x y || collidesWithEnemies enemies x y
     then gameState
     else gameState & gameStateShip .~ Ship x y
  where
    collidesWithStars :: [(Int, Int)] -> Int -> Int -> Bool
    collidesWithStars ss x' y' = (x', y') `elem` ss

    collidesWithEnemies :: [Enemy] -> Int -> Int -> Bool
    collidesWithEnemies _ _ _ = False
