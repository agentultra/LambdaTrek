{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.State where

import Data.Text (Text)
import LambdaTrek.Command
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Ship
import Lens.Micro
import Lens.Micro.TH

data GameState
  = GameState
  { _gameStateCommandInput :: Text
  , _gameStateCommand :: Maybe Command
  , _gameStateCommandError :: Maybe Text
  , _gameStateSector :: Sector
  , _gameStateShip :: Ship
  }
  deriving (Eq, Ord, Show)

makeLenses ''GameState

initialGameState :: GameState
initialGameState
  = GameState
  { _gameStateCommandInput = ""
  , _gameStateCommand = Nothing
  , _gameStateCommandError = Nothing
  , _gameStateSector = emptySector & stars .~ [(10, 10)]
  , _gameStateShip = Ship 2 2
  }
