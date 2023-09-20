{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Ship where

import Lens.Micro
import Lens.Micro.TH

data Ship
  = Ship
  { shipPositionX   :: Int
  , shipPositionY   :: Int
  , shipEnergy      :: Int
  , shipPhaserRange :: Int -- ^ Maximum distance we can target enemies
  , shipHull        :: Int
  }
  deriving (Eq, Ord, Show)

makeFields ''Ship

subtractEnergy :: Int -> Ship -> Ship
subtractEnergy amt ship = ship & energy %~ \oldAmt -> oldAmt - amt
