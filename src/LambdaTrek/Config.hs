{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek.Config where

import Data.Text (Text)

data GameConfig
  = GameConfig
  { _gameConfigNumEnemies          :: Int
    -- ^ Number of enemies to seed the quadrant with
  , _gameConfigNumEnemiesPerSector :: Int
    -- ^ Max number of enemies to place per sector at a time during
    -- Quadrant generation.
  , _gameConfigNumStations         :: Int
    -- ^ Number of stations to seed the quadrant with
  }
  deriving (Eq, Show)

defaultConfig :: GameConfig
defaultConfig
  = GameConfig
  { _gameConfigNumEnemies = 20
  , _gameConfigNumEnemiesPerSector = 3
  , _gameConfigNumStations = 2
  }

validConfig :: GameConfig -> Either Text GameConfig
validConfig config = do
  numEnemies <- inRange "Num of enemies must be between 0 and 255" (1, 255) config._gameConfigNumEnemies
  numStations <- inRange "Num of stations must be between 1 and 5" (1, 8) config._gameConfigNumStations
  pure $ GameConfig numEnemies config._gameConfigNumEnemiesPerSector numStations

inRange :: Text -> (Int, Int) -> Int -> Either Text Int
inRange errMsg (lo, hi) x
  | x >= lo && x <= hi = pure x
  | otherwise = Left errMsg
