{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaTrek.Simulation where

import Control.Monad.State
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import LambdaTrek.Command
import LambdaTrek.Simulation.Combat
import LambdaTrek.Simulation.Dialog
import LambdaTrek.Simulation.Enemy (Enemy (..))
import LambdaTrek.Simulation.Enemy.AI
import LambdaTrek.Simulation.Position
import qualified LambdaTrek.Simulation.Enemy as Enemy
import LambdaTrek.Simulation.Sector
import qualified LambdaTrek.Simulation.Quadrant as Q
import LambdaTrek.Simulation.Station (Station (..))
import qualified LambdaTrek.Simulation.Station as Station
import LambdaTrek.Simulation.Ship (Ship (..), ShieldState (..), WarpFactor (..))
import qualified LambdaTrek.Simulation.Ship as Ship
import LambdaTrek.State
import LambdaTrek.Utils.List
import Lens.Micro
import Lens.Micro.Mtl

updateSimulation :: State GameState ()
updateSimulation = do
  command <- use gameStateCommand
  case command of
    Just cmd -> do
      cmdCost <- turnCost cmd
      commandResult <- handleCommand cmd
      when (commandResult == Performed) $ do
        gameStateRemainingTurns %= flip (-) cmdCost
      gameStateCommand .= Nothing
      updateEnemyStates
      handleEnemies
    _ -> pure ()

turnCost :: Command -> State GameState Int
turnCost = \case
  EngineMove _ _   -> pure 2
  FirePhasers _ _  -> pure 1
  Dock             -> pure 15
  Shields _        -> pure 1
  Transfer _       -> pure 1
  FireTorpedo _ _  -> pure 1
  LongRangeScan _  -> pure 1
  WarpFactor _     -> pure 1
  Warp warpX warpY -> do
    ship <- use gameStateShip
    sectorCoord <- use gameStateSector
    let dist = distance sectorCoord (warpX, warpY)
        warpPercentage = Ship.warpFactorTurnPercent (ship^.Ship.warpFactor)
    pure . floor $ 4.0 * fromIntegral dist * warpPercentage

handleCommand :: Command -> State GameState CommandResult
handleCommand = \case
    EngineMove x y -> handleEngineMove x y
    FirePhasers amt fireMode -> handleFirePhasers amt fireMode
    Dock -> handleDocking
    Shields cmdState -> handleShields cmdState
    Transfer amt -> handleTransfer amt
    FireTorpedo num coords -> handleFireTorpedo num coords
    LongRangeScan coord -> handleLongRangeScan coord
    WarpFactor factor -> handleWarpFactor factor
    Warp x y -> handleWarp x y

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
        , shipShieldStrength = ship_^.Ship.shieldStrength
        , shipTorpedos = ship_^.Ship.torpedos
        , shipWarpFactor = ship_^.Ship.warpFactor
        }
      pure Performed
    _ -> pure Denied

data CollisionResult = Collide | Missed deriving (Eq, Show)

handleStarCollisions :: Int -> Int -> State GameState CollisionResult
handleStarCollisions x y = do
  quadrantStarMap <- use (gameStateQuadrant . Q.quadrantStars)
  currentSector <- use gameStateSector
  let stars_ = quadrantStarMap ! currentSector
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
  quadrantEnemyShipMap <- use (gameStateQuadrant . Q.quadrantEnemyShips)
  currentSector <- use gameStateSector
  let enemies_ = quadrantEnemyShipMap ! currentSector
  case collidesWithEnemies enemies_ x y of
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
  quadrantStationMap <- use (gameStateQuadrant . Q.quadrantStations)
  currentSector <- use gameStateSector
  let stations_ = quadrantStationMap ! currentSector
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
      quadrantStationMap <- use (gameStateQuadrant . Q.quadrantStations)
      currentSector <- use gameStateSector
      let sectorStations' = Array.elems (quadrantStationMap ! currentSector)
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
  priorShieldState <- use (gameStateShip . Ship.shieldState)
  case (priorShieldState, newState) of
    (ShieldsDown, ShieldsUp) -> do
      sayDialog Combat "Raising shields, captain!"
      zoom gameStateShip $ do
        Ship.shieldState .= ShieldsUp
        Ship.energy -= 50
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

handleTransfer :: Int -> State GameState CommandResult
handleTransfer amt = do
  let transferAmount = fromIntegral amt * (0.01 :: Double)
  zoom gameStateShip $ do
    Ship.shieldStrength %= (+ transferAmount)
  sayDialog Engineering
    $ "Increasing shields by " <> Text.pack (show amt)
    <> "% Aye!"
  pure Performed

handleLongRangeScan :: (Int, Int) -> State GameState CommandResult
handleLongRangeScan coord = do
  currentCoord <- use gameStateSector
  if inRange coord currentCoord 2
    then do
    gameStateQuadrant %= Q.scanQuadrant coord
    sayDialog Helm
      $ "Aye, scanning quadrant "
      <> (Text.pack . show $ coord)
      <> " on long range scanners."
    zoom gameStateShip $ do
      Ship.energy -= 10
    pure Performed
    else do
    sayDialog Helm
      $ "Captain, "
      <> (Text.pack . show $ coord)
      <> " is beyond our long range sensor range."
    pure Denied

handleWarpFactor :: WarpFactor -> State GameState CommandResult
handleWarpFactor factor = do
  zoom gameStateShip $ do
    Ship.warpFactor .= factor
  sayDialog Helm $
    "Aye captain, warp factor "
    <> (Text.pack . show . Ship.warpFactorNumeral $ factor)
    <> "."
  pure Performed

handleWarp :: Int -> Int -> State GameState CommandResult
handleWarp warpX warpY = do
  currentSector <- use gameStateSector
  ship <- use gameStateShip
  let warpFactor = ship^.Ship.warpFactor
      dist = distance currentSector (warpX, warpY)
      energyConsumed = (2 ^ dist) * Ship.warpFactorNumeral warpFactor
      coord = (warpX, warpY)
  if energyConsumed <= ship^.Ship.energy
    then do
    gameStateSector .= coord

    quadrant <- use gameStateQuadrant
    let sector' = Q.getSector quadrant coord
        (safeTileX, safeTileY) = findEmpty . buildSectorTiles $ sector'

    zoom gameStateShip $ do
      Ship.energy -= energyConsumed
      Ship.positionX .= safeTileX
      Ship.positionY .= safeTileY
    gameStateQuadrant %= Q.scanQuadrant coord
    sayDialog Helm
      $ "Aye captain, setting course for sector "
      <> (Text.pack . show $ (warpX, warpY))
      <> " at warp factor "
      <> (Text.pack . show $ warpFactor)
    pure Performed
    else do
    sayDialog Helm
      $ "We do not have enough energy to make the jump, "
      <> "captain! I suggest we lower the warp factor or "
      <> "range."
    pure Denied

updateEnemyStates :: State GameState ()
updateEnemyStates = do
  sector' <- getCurrentSector
  forM_ (aliveEnemies sector') updateEnemyState

updateEnemyState :: (Int, Enemy) -> State GameState ()
updateEnemyState e@(_, Enemy {..}) =
  case enemyState of
    Patrolling -> updateEnemyPatrolling e
    Fighting -> updateEnemyFighting e

updateEnemyPatrolling :: (Int, Enemy) -> State GameState ()
updateEnemyPatrolling (enemyIx, enemy) = do
  playerShip <- use gameStateShip
  currentSector <- use gameStateSector
  when (inRange enemy playerShip Enemy.enemyRange) $ do
    zoom gameStateQuadrant $ do
      Q.quadrantEnemyShips %= \enemyShipMap ->
        M.adjust (\enemies -> enemies // [(enemyIx, enemy & Enemy.state .~ Fighting )]) currentSector enemyShipMap

updateEnemyFighting :: (Int, Enemy) -> State GameState ()
updateEnemyFighting (enemyIx, enemy) = do
  currentSector <- use gameStateSector
  ship <- use gameStateShip
  unless (inRange enemy ship Enemy.enemyRange) $ do
    zoom gameStateQuadrant $ do
      Q.quadrantEnemyShips %= \enemyShipMap ->
        M.adjust (\enemies -> enemies // [(enemyIx, enemy & Enemy.state .~ Patrolling)]) currentSector enemyShipMap

handleEnemies :: State GameState ()
handleEnemies = do
  sector' <- getCurrentSector
  forM_ (aliveEnemies sector') handleEnemy

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
    extraDamage <- randomRange 0 5
    damageResult <- damageShip $ enemy^.Enemy.baseDamageAmount + extraDamage
    generateShipDamageDialog damageResult
    else pure ()

data DamageResult
  = DamageResult
  { damageResultHullDamageAmount :: Int
  , damageResultIsDirectHit      :: Bool
  }
  deriving (Eq, Show)

damageShip :: Int -> State GameState DamageResult
damageShip amount = do
  ship <- use gameStateShip
  case ship^.Ship.shieldState of
    ShieldsUp -> do
      let shieldReducedDamageAmount =
            fromIntegral amount * ship^.Ship.shieldStrength
          hullDamageAmount = ceiling
            $ fromIntegral amount - shieldReducedDamageAmount
      zoom gameStateShip $ do
        Ship.hull -= hullDamageAmount
        Ship.shieldStrength -= 0.02
      pure $ DamageResult
        { damageResultHullDamageAmount = hullDamageAmount
        , damageResultIsDirectHit = False
        }
    ShieldsDown -> do
      zoom gameStateShip $
        Ship.hull -= amount
      pure $ DamageResult
        { damageResultHullDamageAmount = amount
        , damageResultIsDirectHit = True
        }

generateShipDamageDialog :: DamageResult -> State GameState ()
generateShipDamageDialog DamageResult {..} = do
  ship <- use gameStateShip
  if damageResultIsDirectHit
    then sayDialog Combat
         $ "We took a direct hit, captain! We lost "
         <> damageAmount damageResultHullDamageAmount
         <> " damage!"
    else
    case ship^.Ship.shieldState of
      ShieldsUp ->
        sayDialog Combat
        $ "Shields holding, we took "
        <> damageAmount damageResultHullDamageAmount
        <> " damage!"
      ShieldsDown ->
        sayDialog Combat "We lost shields, captain! Taking damage!"
  where
    damageAmount :: Int -> Text
    damageAmount amt
      | amt <= 10 = "a little"
      | amt > 10 && amt <= 40 = "some"
      | otherwise = "a lot of"

getCurrentSector :: State GameState Sector
getCurrentSector = do
  quadrant <- use gameStateQuadrant
  currentSector <- use gameStateSector
  pure $ Q.getSector quadrant currentSector

-- | Determine if a coordinate is within @range@ distance.
--
-- @range@ is not inclusive.
inRange :: (HasPosition a , HasPosition b) => a -> b -> Int -> Bool
inRange a b range = distance a b <= range

distance :: (HasPosition a, HasPosition b) => a -> b -> Int
distance x y =
  let (aX, aY) = getPosition x
      (bX, bY) = getPosition y
  in abs (bX - aX) + abs (bY - aY)

findMaybe :: (a -> Maybe a) -> [a] -> Maybe a
findMaybe _ [] = Nothing
findMaybe f (x:xs)
  | isJust $ f x = Just x
  | otherwise = findMaybe f xs

plusOrMinus1 :: Int -> Int -> Bool
plusOrMinus1 x y =
  x - y == 1 || x - y == (-1) || x == y
