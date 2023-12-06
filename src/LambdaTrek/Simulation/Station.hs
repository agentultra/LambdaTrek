{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Station where

import Lens.Micro.TH
import LambdaTrek.Simulation.Position

data Station
  = Station
  { stationPositionX :: Int
  , stationPositionY :: Int
  , stationEnergy :: Int
  }
  deriving (Eq, Ord, Show)

instance HasPosition Station where
  getPosition Station {..} = (stationPositionX, stationPositionY)

makeFields ''Station
