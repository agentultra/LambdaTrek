{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.State where

import Control.Monad.State
import Data.Text (Text)
import LambdaTrek.Command
import LambdaTrek.Config
import LambdaTrek.Simulation.Dialog
import LambdaTrek.Simulation.Quadrant
import LambdaTrek.Simulation.Quadrant.Generate
import LambdaTrek.Simulation.Ship
import LambdaTrek.Simulation.Ship.Generate
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Random

data GameScreen
  = SectorScreen
  | QuadrantScreen
  | GameOverScreen
  deriving (Bounded, Enum, Eq, Show)

data GameState
  = GameState
  { _gameStateCommandInput   :: Text
  , _gameStateCommand        :: Maybe Command
  , _gameStateCommandError   :: Maybe Text
  , _gameStateQuadrant       :: Quadrant
  , _gameStateSector         :: (Int, Int)
  , _gameStateShip           :: Ship
  , _gameStateRemainingTurns :: Int
  , _gameStateDialog         :: [Dialog]
  , _gameStateRandomGen      :: StdGen
  , _gameStateScreen         :: GameScreen
  , _gameStateGameConfig     :: GameConfig
  }
  deriving (Eq, Show)

makeLenses ''GameState

initialGameState :: GameConfig -> StdGen -> GameState
initialGameState config gen =
  let (quadrant, stdGen) = generateQuadrant config gen
      (playerStartingShip, playerQuadrant, stdGen') = generateStartingShip quadrant stdGen
  in GameState
     { _gameStateCommandInput   = ""
     , _gameStateCommand        = Nothing
     , _gameStateCommandError   = Nothing
     , _gameStateQuadrant       = scanQuadrant playerQuadrant quadrant
     , _gameStateSector         = playerQuadrant
     , _gameStateShip           = playerStartingShip
     , _gameStateRemainingTurns = 200
     , _gameStateDialog         = []
     , _gameStateRandomGen      = stdGen'
     , _gameStateScreen         = SectorScreen
     , _gameStateGameConfig     = config
     }

say :: Crewmate -> Text -> [Dialog] -> [Dialog]
say crewmate msg dialogs = Dialog crewmate msg : dialogs

addDialog :: GameState -> Crewmate -> Text -> GameState
addDialog gameState crewmate msg =
  gameState & gameStateDialog %~ say crewmate msg

sayDialog :: Crewmate -> Text -> State GameState ()
sayDialog crewmate msg = gameStateDialog %= say crewmate msg
