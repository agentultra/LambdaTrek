module LambdaTrek.Command where

import LambdaTrek.Units

data Command
  = EngineMove Int Int     -- ^ Move to X / Y of current quadrant
  | JumpMove QuadrantCoord -- ^ Jump to a quadrant
  deriving (Eq, Show)
