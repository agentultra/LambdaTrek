{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.State where

import Data.Text (Text)
import LambdaTrek.Command
import LambdaTrek.Simulation.Sector
import Lens.Micro.TH

data GameState
  = GameState
  { _gameStateCommandInput :: Text
  , _gameStateCommand :: Maybe Command
  , _gameStateSector :: Sector
  }
  deriving (Eq, Ord, Show)

makeLenses ''GameState
