{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaTrek.Simulation where

import qualified Data.Array as Array
import Data.Bifunctor (second)
import qualified Data.List as List
import qualified Data.Text as Text
import LambdaTrek.Command
import LambdaTrek.Simulation.Dialog
import LambdaTrek.Simulation.Enemy (Enemy(..))
import qualified LambdaTrek.Simulation.Enemy as Enemy
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Ship (Ship (..))
import qualified LambdaTrek.Simulation.Ship as Ship
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
  FirePhasers amt fireMode -> handleFirePhasers gameState amt fireMode

handleEngineMove :: GameState -> Int -> Int -> GameState
handleEngineMove gameState x y =
  let stars_ = gameState^.(gameStateSector . stars)
      enemies = gameState^.(gameStateSector . enemyShips)
  in if collidesWithStars stars_ x y
     then addDialog gameState Helm
          ( "Captain, that would take us directly into the star at ("
            <> Text.pack (show x) <> ", " <> Text.pack (show y) <> ")"
          )
     else if collidesWithEnemies (Array.elems enemies) x y
          then addDialog gameState Helm
               ( "Captain, we would collide directly with the enemy ship at ("
                 <> Text.pack (show x) <> ", " <> Text.pack (show y) <> ")"
               )
          else gameState & gameStateShip .~ Ship x y (gameState^.gameStateShip.Ship.energy - 2) (gameState^.gameStateShip.Ship.phaserRange)
  where
    collidesWithStars :: [(Int, Int)] -> Int -> Int -> Bool
    collidesWithStars ss x' y' = (x', y') `elem` ss

    collidesWithEnemies :: [Enemy] -> Int -> Int -> Bool
    collidesWithEnemies enemies x' y' = case List.find (collidesWithEnemy x' y') enemies of
      Nothing -> False
      Just _ -> True

    collidesWithEnemy :: Int -> Int -> Enemy -> Bool
    collidesWithEnemy x' y' enemy =
      enemy^.Enemy.positionX == x' && enemy^.Enemy.positionY == y'

handleFirePhasers :: GameState -> Int -> PhaserMode -> GameState
handleFirePhasers gameState energyAmt firingMode =
  let Ship {..} = gameState^.gameStateShip
      rangeBoxCorner = ( min 0 (shipPositionX - shipPhaserRange)
                       , min 0 (shipPositionY - shipPhaserRange))
      rangeBoxOffset = (max 15 (shipPositionX + shipPhaserRange)
                       , max 15 (shipPositionY + shipPhaserRange))
      enemiesInRange
        = filter (enemyInRange rangeBoxCorner rangeBoxOffset)
        . Array.assocs
        $ gameState^.(gameStateSector . enemyShips)
  in case firingMode of
    PhaserAutomatic ->
      let damageAmount = energyAmt `div` length enemiesInRange
          damagedEnemies = map (second $ Enemy.damageEnemy damageAmount) enemiesInRange
          gameState' = gameState & gameStateShip %~ Ship.subtractEnergy (damageAmount * length damagedEnemies)
      in gameState' & (gameStateSector . enemyShips) %~ \ships -> ships Array.// damagedEnemies
    PhaserManual -> gameState
  where
    -- TODO (james): this appears it might have a bug in it, can
    -- target enemies out of range
    enemyInRange :: (Int, Int) -> (Int, Int) -> (Int, Enemy) -> Bool
    enemyInRange (topLeftX, topLeftY) (offSetX, offSetY) (_, Enemy {..}) =
      enemyPositionX >= topLeftX
      && (topLeftX + offSetX) >= enemyPositionX
      && enemyPositionY >= topLeftY
      && (topLeftY + offSetY) >= enemyPositionY
