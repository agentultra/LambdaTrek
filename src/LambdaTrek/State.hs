{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.State where

import Control.Monad.State
import Data.Text (Text)
import LambdaTrek.Command
import LambdaTrek.Simulation.Dialog
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Ship
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Random

data GameState
  = GameState
  { _gameStateCommandInput :: Text
  , _gameStateCommand :: Maybe Command
  , _gameStateCommandError :: Maybe Text
  , _gameStateSector :: Sector
  , _gameStateShip :: Ship
  , _gameStateDialog :: [Dialog]
  , _gameStateRandomGen :: StdGen
  }
  deriving (Eq, Show)

makeLenses ''GameState

initialGameState :: StdGen -> GameState
initialGameState gen
  = GameState
  { _gameStateCommandInput = ""
  , _gameStateCommand = Nothing
  , _gameStateCommandError = Nothing
  , _gameStateSector = emptySector & stars .~ [(10, 10)]
  , _gameStateShip = Ship 2 2 100 6
  , _gameStateDialog = []
  , _gameStateRandomGen = gen
  }

say :: Crewmate -> Text -> [Dialog] -> [Dialog]
say crewmate msg dialogs = Dialog crewmate msg : dialogs

addDialog :: GameState -> Crewmate -> Text -> GameState
addDialog gameState crewmate msg =
  gameState & gameStateDialog %~ say crewmate msg

sayDialog :: Crewmate -> Text -> State GameState ()
sayDialog crewmate msg = gameStateDialog %= say crewmate msg
