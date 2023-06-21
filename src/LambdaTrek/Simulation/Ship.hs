module LambdaTrek.Simulation.Ship where

data Ship
  = Ship
  { _shipX :: Int
  , _shipY :: Int
  }
  deriving (Eq, Ord, Show)
