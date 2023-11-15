{-# LANGUAGE LambdaCase #-}

module LambdaTrek.Command where

import LambdaTrek.Simulation.Ship

data PhaserMode = PhaserAutomatic | PhaserManual
  deriving (Eq, Ord, Show)

data Command
  = EngineMove Int Int     -- ^ Move to X / Y of current quadrant
  | FirePhasers Int PhaserMode
  | Dock
  | Shields ShieldState
  | Transfer Int
    -- ^ Transfer X amount of energy to the shields to raise shield strength
  | FireTorpedo Int [(Int, Int)]
    -- ^ Fire X number of torpedos at specified targets, one target per torpedo
  | LongRangeScan (Int, Int)
  | WarpFactor WarpFactor
  | Warp Int Int
  deriving (Eq, Ord, Show)

-- | The result of executing a command may change state.  If it does
-- change the state, 'Performed' indicates that game time has elapsed
-- and 'Denied' means that the command was not executed.
data CommandResult = Performed | Denied deriving (Eq, Show)
