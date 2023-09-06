module LambdaTrek.Command where

import LambdaTrek.Units

data PhaserMode = PhaserAutomatic | PhaserManual
  deriving (Eq, Ord, Show)

data Command
  = EngineMove Int Int     -- ^ Move to X / Y of current quadrant
  | JumpMove QuadrantCoord -- ^ Jump to a quadrant
  | FirePhasers Int PhaserMode
  | Dock
  deriving (Eq, Ord, Show)
