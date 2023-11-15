{-# LANGUAGE LambdaCase #-}

module LambdaTrek.Command where

import LambdaTrek.Units
import LambdaTrek.Simulation.Ship

data PhaserMode = PhaserAutomatic | PhaserManual
  deriving (Eq, Ord, Show)

data Command
  = EngineMove Int Int     -- ^ Move to X / Y of current quadrant
  | JumpMove QuadrantCoord -- ^ Jump to a quadrant
  | FirePhasers Int PhaserMode
  | Dock
  | Shields ShieldState
  | Transfer Int
    -- ^ Transfer X amount of energy to the shields to raise shield strength
  | FireTorpedo Int [(Int, Int)]
    -- ^ Fire X number of torpedos at specified targets, one target per torpedo
  | LongRangeScan (Int, Int)
  | WarpFactor WarpFactor
  deriving (Eq, Ord, Show)

turnCost :: Command -> Int
turnCost = \case
  EngineMove _ _ -> 2
  JumpMove _ -> 0 -- TODO: Update when we implement JUMP
  FirePhasers _ _ -> 1
  Dock -> 15
  Shields _ -> 1
  Transfer _ -> 1
  FireTorpedo _ _ -> 1
  LongRangeScan _ -> 1
  WarpFactor _ -> 1

-- | The result of executing a command may change state.  If it does
-- change the state, 'Performed' indicates that game time has elapsed
-- and 'Denied' means that the command was not executed.
data CommandResult = Performed | Denied deriving (Eq, Show)
