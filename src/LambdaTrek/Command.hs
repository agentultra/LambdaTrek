{-# LANGUAGE LambdaCase #-}

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

turnCost :: Command -> Int
turnCost = \case
  EngineMove _ _ -> 2
  JumpMove _ -> 0 -- TODO: Update when we implement JUMP
  FirePhasers _ _ -> 1
  Dock -> 5

-- | The result of executing a command may change state.  If it does
-- change the state, 'Performed' indicates that game time has elapsed
-- and 'Denied' means that the command was not executed.
data CommandResult = Performed | Denied deriving (Eq, Show)
