{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaTrek.Simulation.Quadrant.Generate where

import Control.Monad
import Control.Monad.State
import Data.Foldable
import System.Random
import LambdaTrek.Simulation.Enemy
import LambdaTrek.Simulation.Enemy.AI
import LambdaTrek.Simulation.Quadrant
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Station

data GenerationState
  = GenerationState
  { generationRandomGen   :: StdGen
  , generationQuadrant    :: Quadrant
  , generationStationsMax :: Int
  }

genRandom :: State GenerationState Int
genRandom = do
  genState <- get
  let (x, gen') = uniformR (0, 14) genState.generationRandomGen
  put $ genState { generationRandomGen = gen' }
  pure x

genRandomBetween :: (Int, Int) -> State GenerationState Int
genRandomBetween bs = do
  genState <- get
  let (x, gen') = uniformR bs genState.generationRandomGen
  put $ genState { generationRandomGen = gen' }
  pure x

genPickRandom :: [a] -> State GenerationState (Maybe a)
genPickRandom [] = pure Nothing
genPickRandom xs = do
  randomIx <- genRandomBetween (0, length xs)
  pure $ Just (xs !! randomIx)

addSectorStar :: (Int, Int) -> (Int, Int) -> State GenerationState (Maybe (Int, Int))
addSectorStar sectorCoord starCoord = do
  genState@GenerationState {..} <- get
  case addStar sectorCoord starCoord generationQuadrant of
    Nothing -> pure Nothing
    Just quadrant -> do
      put $ genState { generationQuadrant = quadrant }
      pure $ Just starCoord

addSectorEnemy :: (Int, Int) -> Enemy -> State GenerationState (Maybe Enemy)
addSectorEnemy sectorCoord enemy = do
  genState@GenerationState {..} <- get
  case addEnemy sectorCoord enemy generationQuadrant of
    Nothing -> pure Nothing
    Just quadrant -> do
      put $ genState { generationQuadrant = quadrant }
      pure $ Just enemy

addSectorStation :: (Int, Int) -> Station -> State GenerationState (Maybe Station)
addSectorStation sectorCoord station = do
  genState@GenerationState {..} <- get
  case addStation sectorCoord station generationQuadrant of
    Nothing -> pure Nothing
    Just quadrant -> do
      put $ genState { generationQuadrant = quadrant }
      pure $ Just station

generateQuadrant :: (Int, Int) -> StdGen -> (Quadrant, StdGen)
generateQuadrant shipStartingCoord randGen =
  let GenerationState {..} =
        -- TODO (james): move the max num stations to a param
        (`execState` (GenerationState randGen (initQuadrant shipStartingCoord)) 2) $ do
        generateQuadrantStars
        generateQuadrantEnemyShips
        generateStations
  in (generationQuadrant, generationRandomGen)

generateQuadrantStars :: State GenerationState ()
generateQuadrantStars = traverse_ generateSectorStars quadrantCoords

generateSectorStars :: (Int, Int) -> State GenerationState ()
generateSectorStars coord = do
  numStars <- genRandomBetween (0, 5)
  starsX <- traverse (const genRandom) [0..numStars]
  starsY <- traverse (const genRandom) [0..numStars]

  forM_ (zip starsX starsY) $ \star ->
    addSectorStar coord star

generateQuadrantEnemyShips :: State GenerationState ()
generateQuadrantEnemyShips = traverse_ generateEnemyShips quadrantCoords

generateEnemyShips :: (Int, Int) -> State GenerationState ()
generateEnemyShips coord = do
  numEnemies <- genRandomBetween (1, 4)
  enemiesX <- traverse (const genRandom) [0..numEnemies]
  enemiesY <- traverse (const genRandom) [0..numEnemies]

  forM_ (zip enemiesX enemiesY) $ \(eX, eY) -> do
    let enemy
          = Enemy
          { enemyPositionX        = eX
          , enemyPositionY        = eY
          , enemyHitPoints        = 10
          , enemyShieldValue      = 20
          , enemyState            = Patrolling
          , enemyBaseDamageAmount = 10
          }

    addSectorEnemy coord enemy

generateStations :: State GenerationState ()
generateStations = do
  goGenerateStations 0 $ cycle quadrantCoords

goGenerateStations :: Int -> [(Int, Int)] -> State GenerationState ()
goGenerateStations _ [] = pure ()
goGenerateStations stationCount (sectorCoord:coords) = do
  GenerationState {..} <- get
  when (stationCount /= generationStationsMax) $ do
    stationPlacementChance <- genRandomBetween (0, 1)
    if stationPlacementChance == 1
      then placeStation sectorCoord >> goGenerateStations (stationCount + 1) coords
      else goGenerateStations stationCount coords
  pure ()

placeStation :: (Int, Int) -> State GenerationState ()
placeStation sectorCoord = do
  -- TODO (james) randomize the station position from the empty sector positions.
  quadrant <- generationQuadrant <$> get
  let sector' = getSector quadrant sectorCoord
  maybePosition <- genPickRandom $ emptyCoords sector'
  case maybePosition of
    Nothing -> pure ()
    Just (stationX, stationY) -> do
      let station = Station stationX stationY 100
      _ <- addSectorStation sectorCoord station
      pure ()
