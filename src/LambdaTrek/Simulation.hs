{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek.Simulation where

import Control.Monad.State
import qualified Data.Array as Array
import Data.Maybe (isJust)
import qualified Data.Text as Text
import LambdaTrek.Command
import LambdaTrek.Simulation.Combat
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
  FirePhasers amt fireMode -> (`execState` gameState) $ handleFirePhasers amt fireMode

handleEngineMove :: GameState -> Int -> Int -> GameState
handleEngineMove gameState x y =
  let stars_ = gameState^.(gameStateSector . stars)
      enemies = gameState^.(gameStateSector . enemyShips)
  in if collidesWithStars stars_ x y
     then addDialog gameState Helm
          ( "Captain, that would take us directly into the star at ("
            <> Text.pack (show x) <> ", " <> Text.pack (show y) <> ")"
          )
     else case collidesWithEnemies (Array.elems enemies) x y of
            Just e | Enemy.isDestroyed e ->
                     addDialog gameState Helm
                     ( "Captain, we would collide directly with volatile ship debris at ("
                       <> Text.pack (show x) <> ", " <> Text.pack (show y) <> ")"
                     )
            Just _ | otherwise ->
                     addDialog gameState Helm
                     ( "Captain, we would collide directly with the enemy ship at ("
                       <> Text.pack (show x) <> ", " <> Text.pack (show y) <> ")"
                     )
            Nothing -> gameState & gameStateShip .~ Ship x y (gameState^.gameStateShip.Ship.energy - 2) (gameState^.gameStateShip.Ship.phaserRange)
  where
    collidesWithStars :: [(Int, Int)] -> Int -> Int -> Bool
    collidesWithStars ss x' y' = (x', y') `elem` ss

    collidesWithEnemies :: [Enemy] -> Int -> Int -> Maybe Enemy
    collidesWithEnemies enemies x' y' = findMaybe (collidesWithEnemy x' y') enemies

    collidesWithEnemy :: Int -> Int -> Enemy -> Maybe Enemy
    collidesWithEnemy x' y' enemy
      | enemy^.Enemy.positionX == x'
        && enemy^.Enemy.positionY == y' = Just enemy
      | otherwise = Nothing

findMaybe :: (a -> Maybe a) -> [a] -> Maybe a
findMaybe _ [] = Nothing
findMaybe f (x:xs)
  | isJust $ f x = Just x
  | otherwise = findMaybe f xs
