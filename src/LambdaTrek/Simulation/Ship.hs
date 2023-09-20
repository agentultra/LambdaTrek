{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Ship where

import Data.Text (Text)
import Lens.Micro
import Lens.Micro.TH

data ShieldState = ShieldsUp | ShieldsDown
  deriving (Eq, Ord, Show)

shieldStateText :: ShieldState -> Text
shieldStateText = \case
  ShieldsUp -> "Up"
  ShieldsDown -> "Down"

data Ship
  = Ship
  { shipPositionX   :: Int
  , shipPositionY   :: Int
  , shipEnergy      :: Int
  , shipPhaserRange :: Int -- ^ Maximum distance we can target enemies
  , shipHull        :: Int
  , shipShieldState :: ShieldState
  }
  deriving (Eq, Ord, Show)

makeFields ''Ship

subtractEnergy :: Int -> Ship -> Ship
subtractEnergy amt ship = ship & energy %~ \oldAmt -> oldAmt - amt
