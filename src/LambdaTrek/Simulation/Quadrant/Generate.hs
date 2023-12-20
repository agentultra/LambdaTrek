module LambdaTrek.Simulation.Quadrant.Generate where

import Control.Monad.State
import Data.Array
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Random
import LambdaTrek.Simulation.Enemy
import LambdaTrek.Simulation.Quadrant
import LambdaTrek.Simulation.Station

newtype GenerationState = GenerationState { generationRandomGen :: StdGen }

genRandom :: State GenerationState Int
genRandom = do
  (GenerationState gen) <- get
  let (x, gen') = uniformR (0, 14) gen
  put (GenerationState gen')
  pure x

genRandomBetween :: (Int, Int) -> State GenerationState Int
genRandomBetween bs = do
  (GenerationState gen) <- get
  let (x, gen') = uniformR bs gen
  put (GenerationState gen')
  pure x

generateQuadrant :: State GenerationState Quadrant
generateQuadrant = do
  generatedStars <- generateQuadrantStars
  generatedEnemyShips <- generateEnemyShips
  generatedStations <- generateStations
  let quadrant = (initQuadrant (0, 0))
        { _quadrantStars = generatedStars
        , _quadrantEnemyShips = generatedEnemyShips
        , _quadrantStations = generatedStations
        }
  pure quadrant

generateQuadrantStars :: State GenerationState (Map (Int, Int) [(Int, Int)])
generateQuadrantStars = do
  stars <- traverse generateSectorStars quadrantCoords
  pure $ Map.fromList stars

generateSectorStars :: (Int, Int) -> State GenerationState ((Int, Int), [(Int, Int)])
generateSectorStars coord = do
  numStars <- genRandomBetween (0, 5)
  starsX <- traverse (const genRandom) [0..numStars]
  starsY <- traverse (const genRandom) [0..numStars]

  pure (coord, zip starsX starsY)

generateEnemyShips :: State GenerationState (Map (Int, Int) (Array Int Enemy))
generateEnemyShips = pure $ Map.fromList [(coord, emptyEnemyArray) | coord <- quadrantCoords]
  where
    emptyEnemyArray = listArray (0, -1) []

generateStations :: State GenerationState (Map (Int, Int) (Array Int Station))
generateStations = pure $ Map.fromList [(coord, emptyStationArray) | coord <- quadrantCoords]
  where
    emptyStationArray = listArray (0, -1) []
