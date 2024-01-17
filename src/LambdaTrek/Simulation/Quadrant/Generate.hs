{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaTrek.Simulation.Quadrant.Generate where

import Control.Monad.State
import Data.Array
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Random
import LambdaTrek.Simulation.Enemy
import LambdaTrek.Simulation.Enemy.AI
import LambdaTrek.Simulation.Quadrant
import LambdaTrek.Simulation.Station

data GenerationState
  = GenerationState
  { generationRandomGen :: StdGen
  , generationQuadrant  :: Quadrant
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

generateQuadrant :: (Int, Int) -> StdGen -> (Quadrant, StdGen)
generateQuadrant shipStartingCoord randGen =
  let GenerationState {..} =
        (`execState` (GenerationState randGen (initQuadrant shipStartingCoord))) $ do
        generateQuadrantStars
        generateQuadrantEnemyShips
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

generateStations :: State GenerationState (Map (Int, Int) (Array Int Station))
generateStations = pure $ Map.fromList [(coord, emptyStationArray) | coord <- quadrantCoords]
  where
    emptyStationArray = listArray (0, -1) []
