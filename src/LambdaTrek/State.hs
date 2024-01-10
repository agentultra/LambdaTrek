{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.State where

import Control.Monad.State
import Data.Text (Text)
import LambdaTrek.Command
import LambdaTrek.Simulation.Dialog
import LambdaTrek.Simulation.Quadrant
import LambdaTrek.Simulation.Quadrant.Generate
import LambdaTrek.Simulation.Ship
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Random

data GameScreen
  = SectorScreen
  | QuadrantScreen
  | GameOverScreen
  deriving (Bounded, Enum, Eq, Show)

data GameConfig
  = GameConfig
  { _gameConfigNumEnemies :: Int
    -- ^ Number of enemies to seed the quadrant with
  }
  deriving (Eq, Show)

data GameState
  = GameState
  { _gameStateCommandInput :: Text
  , _gameStateCommand :: Maybe Command
  , _gameStateCommandError :: Maybe Text
  , _gameStateQuadrant :: Quadrant
  , _gameStateSector :: (Int, Int)
  , _gameStateShip :: Ship
  , _gameStateRemainingTurns :: Int
  , _gameStateDialog :: [Dialog]
  , _gameStateRandomGen :: StdGen
  , _gameStateScreen :: GameScreen
  , _gameStateGameConfig :: GameConfig
  }
  deriving (Eq, Show)

makeLenses ''GameState

initialGameState :: StdGen -> GameState
initialGameState gen =
  let (quadrant, stdGen) = generateQuadrant (0, 0) gen
  in GameState
     { _gameStateCommandInput = ""
     , _gameStateCommand = Nothing
     , _gameStateCommandError = Nothing
     , _gameStateQuadrant = quadrant
     , _gameStateSector = (0, 0)
     , _gameStateShip = Ship 2 2 100 6 30 ShieldsDown 0.75 5 WarpFactorOne
     , _gameStateRemainingTurns = 200
     , _gameStateDialog = []
     , _gameStateRandomGen = stdGen
     , _gameStateScreen = SectorScreen
     , _gameStateGameConfig = GameConfig { _gameConfigNumEnemies = 15 }
     }

say :: Crewmate -> Text -> [Dialog] -> [Dialog]
say crewmate msg dialogs = Dialog crewmate msg : dialogs

addDialog :: GameState -> Crewmate -> Text -> GameState
addDialog gameState crewmate msg =
  gameState & gameStateDialog %~ say crewmate msg

sayDialog :: Crewmate -> Text -> State GameState ()
sayDialog crewmate msg = gameStateDialog %= say crewmate msg
