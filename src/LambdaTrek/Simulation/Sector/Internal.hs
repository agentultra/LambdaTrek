{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Sector.Internal where

import Data.Array
import LambdaTrek.Simulation.Enemy
import LambdaTrek.Simulation.Station (Station (..))
import Lens.Micro.TH

-- | Sectors are 15x15 tiled regions of space
--
-- Be careful not to fill up the sector so there's nowhere to put/move
-- the ship! Leave at least one empty space!
data Sector
  = Sector
  { sectorStars      :: [(Int, Int)]
  , sectorEnemyShips :: Array Int Enemy
  , sectorStations   :: Array Int Station
  }
  deriving (Eq, Ord, Show)

makeFields ''Sector

listEnemies :: Sector -> [Enemy]
listEnemies Sector {..} = elems sectorEnemyShips

listStars :: Sector -> [(Int, Int)]
listStars Sector {..} = sectorStars

listStations :: Sector -> [Station]
listStations Sector {..} = elems sectorStations

hasAtLeastOneEmptySpace :: Sector -> Bool
hasAtLeastOneEmptySpace sector =
  let numStars = length $ listStars sector
      numEnemies = length $ listEnemies sector
      numStations = length $ listStations sector
  in numStars + numEnemies + numStations <= 15 * 15 - 1
