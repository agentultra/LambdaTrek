{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaTrek.Simulation where

import Control.Monad.State
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.Text as Text
import LambdaTrek.Command
import LambdaTrek.Simulation.Combat
import LambdaTrek.Simulation.Dialog
import LambdaTrek.Simulation.Enemy (Enemy (..))
import LambdaTrek.Simulation.Enemy.AI
import qualified LambdaTrek.Simulation.Enemy as Enemy
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Station (Station (..))
import qualified LambdaTrek.Simulation.Station as Station
import LambdaTrek.Simulation.Ship (Ship (..), ShieldState (..))
import qualified LambdaTrek.Simulation.Ship as Ship
import LambdaTrek.State
import Lens.Micro
import Lens.Micro.Mtl

updateSimulation :: State GameState ()
updateSimulation = do
  command <- use gameStateCommand
  case command of
    Just cmd -> do
      commandResult <- handleCommand cmd
      when (commandResult == Performed) $ do
        gameStateRemainingTurns %= flip (-) (turnCost cmd)
      gameStateCommand .= Nothing
      updateEnemyStates
      handleEnemies
    _ -> pure ()

handleCommand :: Command -> State GameState CommandResult
handleCommand = \case
    EngineMove x y -> handleEngineMove x y
    JumpMove _ -> pure Denied -- TODO: not implemented
    FirePhasers amt fireMode -> handleFirePhasers amt fireMode
    Dock -> handleDocking
    Shields cmdState -> handleShields cmdState

handleEngineMove :: Int -> Int -> State GameState CommandResult
handleEngineMove x y = do
  ship_ <- use gameStateShip
  didCollideWithStars <- handleStarCollisions x y
  didCollideWithEnemies <- handleEnemyCollisions x y
  didCollideWithStations <- handleStationCollisions x y
  case (didCollideWithStars, didCollideWithEnemies, didCollideWithStations) of
    (Missed, Missed, Missed) -> do
      gameStateShip .= Ship
        { shipPositionX = x
        , shipPositionY = y
        , shipEnergy = ship_^.Ship.energy - 2
        , shipPhaserRange = ship_^.Ship.phaserRange
        , shipHull = ship_^.Ship.hull
        , shipShieldState = ship_^.Ship.shieldState
        }
      pure Performed
    _ -> pure Denied

data CollisionResult = Collide | Missed deriving (Eq, Show)

handleStarCollisions :: Int -> Int -> State GameState CollisionResult
handleStarCollisions x y = do
  stars_ <- use (gameStateSector . stars)
  if collidesWithStars stars_ x y
    then do
    sayDialog Helm
      ( "Captain, that would take us directly into the star at ("
        <> Text.pack (show x) <> ", " <> Text.pack (show y) <> ")"
      )
    pure Collide
    else pure Missed
  where
    collidesWithStars :: [(Int, Int)] -> Int -> Int -> Bool
    collidesWithStars ss x' y' = (x', y') `elem` ss

handleEnemyCollisions :: Int -> Int -> State GameState CollisionResult
handleEnemyCollisions x y = do
  enemies_ <- use (gameStateSector . enemyShips)
  case collidesWithEnemies (Array.elems enemies_) x y of
    Just e | Enemy.isDestroyed e -> do
               sayDialog Helm
                 ( "Captain, we would collide directly with volatile ship debris at ("
                   <> Text.pack (show x) <> ", " <> Text.pack (show y) <> ")"
                 )
               pure Collide
    Just _ | otherwise -> do
               sayDialog Helm
                 ( "Captain, we would collide directly with the enemy ship at ("
                   <> Text.pack (show x) <> ", " <> Text.pack (show y) <> ")"
                 )
               pure Collide
    Nothing -> pure Missed
  where
    collidesWithEnemies :: [Enemy] -> Int -> Int -> Maybe Enemy
    collidesWithEnemies enemies x' y' = findMaybe (collidesWithEnemy x' y') enemies

    collidesWithEnemy :: Int -> Int -> Enemy -> Maybe Enemy
    collidesWithEnemy x' y' enemy
      | enemy^.Enemy.positionX == x'
        && enemy^.Enemy.positionY == y' = Just enemy
      | otherwise = Nothing

handleStationCollisions :: Int -> Int -> State GameState CollisionResult
handleStationCollisions x y = do
  stations_ <- use (gameStateSector . stations)
  case collidesWithStations (Array.elems stations_) x y of
    Just _ -> do
      sayDialog Helm
        ( "Captain, we would collide with the station at ("
          <> Text.pack (show x) <> ", " <> Text.pack (show y) <> ")"
        )
      pure Collide
    Nothing -> pure Missed
  where
    collidesWithStations :: [Station] -> Int -> Int -> Maybe Station
    collidesWithStations stations' x' y' = findMaybe (collidesWithStation x' y') stations'

    collidesWithStation :: Int -> Int -> Station -> Maybe Station
    collidesWithStation x' y' station
      | station^.Station.positionX == x'
        && station^.Station.positionY == y' = Just station
      | otherwise = Nothing

handleDocking :: State GameState CommandResult
handleDocking = do
  maybeStation <- findStation
  case maybeStation of
    Nothing -> do
      sayDialog Helm "There is no starbase to dock at nearby, captain."
      pure Denied
    Just station -> do
      zoom gameStateShip $ do
        Ship.energy .= 100
        Ship.hull .= 10
      sayDialog Helm
        ("Replenishing supplies at station ("
         <> Text.pack (show (station^.Station.positionX))
         <> ", "
         <> Text.pack (show (station^.Station.positionY))
         <> "), sir!"
        )
      pure Performed
  where
    findStation :: State GameState (Maybe Station)
    findStation = do
      ship <- use gameStateShip
      sectorStations' <- Array.elems <$> use (gameStateSector . stations)
      pure . List.find (isAdjacent ship) $ sectorStations'

    isAdjacent :: Ship -> Station -> Bool
    isAdjacent ship station =
      let shipX = ship^.Ship.positionX
          shipY = ship^.Ship.positionY
          stationX = station^.Station.positionX
          stationY = station^.Station.positionY
      in plusOrMinus1 shipX stationX && plusOrMinus1 shipY stationY

handleShields :: ShieldState -> State GameState CommandResult
handleShields newState = do
  shieldState <- use (gameStateShip . Ship.shieldState)
  case (shieldState, newState) of
    (ShieldsDown, ShieldsUp) -> do
      sayDialog Combat "Raising shields, captain!"
      zoom gameStateShip $
        Ship.shieldState .= ShieldsUp
      pure Performed
    (ShieldsUp, ShieldsUp) -> do
      sayDialog Combat "Shields are up, captain!"
      pure Denied
    (ShieldsDown, ShieldsDown) -> do
      sayDialog Combat "Shields are down, captain!"
      pure Denied
    (ShieldsUp, ShieldsDown) -> do
      sayDialog Combat "Lowering shields, captain!"
      zoom gameStateShip $
        Ship.shieldState .= ShieldsDown
      pure Performed

updateEnemyStates :: State GameState ()
updateEnemyStates = do
  sector <- use gameStateSector
  forM_ (aliveEnemies sector) updateEnemyState

updateEnemyState :: (Int, Enemy) -> State GameState ()
updateEnemyState e@(_, Enemy {..}) =
  case enemyState of
    Patrolling -> updateEnemyPatrolling e
    Fighting -> updateEnemyFighting e

updateEnemyPatrolling :: (Int, Enemy) -> State GameState ()
updateEnemyPatrolling (enemyIx, enemy) = do
  playerShip <- use gameStateShip
  when (inRange enemy playerShip Enemy.enemyRange) $ do
    zoom gameStateSector $ do
      enemyShips %= \enemies -> enemies Array.// [(enemyIx, enemy & Enemy.state .~ Fighting )]

updateEnemyFighting :: (Int, Enemy) -> State GameState ()
updateEnemyFighting (enemyIx, enemy) = do
  ship <- use gameStateShip
  unless (inRange enemy ship Enemy.enemyRange) $ do
    zoom gameStateSector $ do
      enemyShips %= \enemies -> enemies Array.// [(enemyIx, enemy & Enemy.state .~ Patrolling)]

handleEnemies :: State GameState ()
handleEnemies = do
  sector <- use gameStateSector
  forM_ (aliveEnemies sector) handleEnemy

handleEnemy :: (Int, Enemy) -> State GameState ()
handleEnemy e@(_, Enemy {..}) = case enemyState of
  Patrolling -> handleEnemyPatrolling e
  Fighting -> handleEnemyFighting e

handleEnemyPatrolling :: (Int, Enemy) -> State GameState ()
handleEnemyPatrolling _ = pure ()

handleEnemyFighting :: (Int, Enemy) -> State GameState ()
handleEnemyFighting (_, enemy) = do
  ship <- use gameStateShip
  if inRange enemy ship Enemy.enemyRange
    then do
    sayDialog Combat "We took a direct hit!"
    zoom gameStateShip $ do
      Ship.hull %= \oldHull -> oldHull - 1
    else pure ()

class HasPosition a where
  getPosition :: a -> (Int, Int)

instance HasPosition Enemy where
  getPosition Enemy {..} = (enemyPositionX, enemyPositionY)

instance HasPosition Ship where
  getPosition Ship {..} = (shipPositionX, shipPositionY)

inRange :: (HasPosition a , HasPosition b) => a -> b -> Int -> Bool
inRange a b range =
  let (aX, aY) = getPosition a
      (bX, bY) = getPosition b
      distance = abs (bX - aX) + abs (bY - aY)
  in distance <= range

findMaybe :: (a -> Maybe a) -> [a] -> Maybe a
findMaybe _ [] = Nothing
findMaybe f (x:xs)
  | isJust $ f x = Just x
  | otherwise = findMaybe f xs

plusOrMinus1 :: Int -> Int -> Bool
plusOrMinus1 x y =
  x - y == 1 || x - y == (-1) || x == y
