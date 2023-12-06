{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Ship where

import Data.Text (Text)
import LambdaTrek.Simulation.Position
import Lens.Micro
import Lens.Micro.TH

data ShieldState = ShieldsUp | ShieldsDown
  deriving (Eq, Ord, Show)

shieldStateText :: ShieldState -> Text
shieldStateText = \case
  ShieldsUp -> "Up"
  ShieldsDown -> "Down"

data WarpFactor
  = WarpFactorOne
  | WarpFactorTwo
  | WarpFactorThree
  | WarpFactorFour
  | WarpFactorFive
  deriving (Bounded, Enum, Eq, Ord, Show)

warpFactorNumeral :: WarpFactor -> Int
warpFactorNumeral = (+ 1) . fromEnum

data Ship
  = Ship
  { shipPositionX      :: Int
  , shipPositionY      :: Int
  , shipEnergy         :: Int
  , shipPhaserRange    :: Int -- ^ Maximum distance we can target enemies
  , shipHull           :: Int
  , shipShieldState    :: ShieldState
  , shipShieldStrength :: Double -- ^ Between 0 and 1 representing a
                                 -- percentage of damage reduction.
  , shipTorpedos       :: Int
  , shipWarpFactor     :: WarpFactor
  }
  deriving (Eq, Ord, Show)

instance HasPosition Ship where
  getPosition Ship {..} = (shipPositionX, shipPositionY)

makeFields ''Ship

subtractEnergy :: Int -> Ship -> Ship
subtractEnergy amt ship = ship & energy %~ \oldAmt -> oldAmt - amt

warpFactorTurnPercent :: WarpFactor -> Double
warpFactorTurnPercent = \case
  WarpFactorOne   -> 1.0
  WarpFactorTwo   -> 0.9
  WarpFactorThree -> 0.8
  WarpFactorFour  -> 0.7
  WarpFactorFive  -> 0.6
