module LambdaTrek.Config where

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
