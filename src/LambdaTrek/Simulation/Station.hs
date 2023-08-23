{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Station where

import Lens.Micro.TH

data Station
  = Station
  { stationPositionX :: Int
  , stationPositionY :: Int
  , stationEnergy :: Int
  }
  deriving (Eq, Ord, Show)

makeFields ''Station
