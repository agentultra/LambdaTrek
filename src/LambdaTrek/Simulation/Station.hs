module LambdaTrek.Simulation.Station where

data Station
  = Station
  { stationPositionX :: Int
  , stationPositionY :: Int
  , stationEnergy :: Int
  }
  deriving (Eq, Ord, Show)
