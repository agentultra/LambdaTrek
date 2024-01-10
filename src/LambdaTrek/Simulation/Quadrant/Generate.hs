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

generateQuadrant :: (Int, Int) -> StdGen -> (Quadrant, StdGen)
generateQuadrant shipStartingCoord randGen =
  let GenerationState {..} =
        (`execState` (GenerationState randGen (initQuadrant shipStartingCoord))) $ do
        generateQuadrantStars
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

generateEnemyShips :: State GenerationState (Map (Int, Int) (Array Int Enemy))
generateEnemyShips = pure $ Map.fromList [(coord, emptyEnemyArray) | coord <- quadrantCoords]
  where
    emptyEnemyArray = listArray (0, -1) []

generateStations :: State GenerationState (Map (Int, Int) (Array Int Station))
generateStations = pure $ Map.fromList [(coord, emptyStationArray) | coord <- quadrantCoords]
  where
    emptyStationArray = listArray (0, -1) []
