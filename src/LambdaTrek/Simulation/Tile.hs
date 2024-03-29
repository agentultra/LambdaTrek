{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek.Simulation.Tile where

import Data.Text (Text)

data Tile
  = PlayerShip
  | EnemyShip
  | DestroyedEnemyShip
  | EmptySpace
  | Star
  | Station
  deriving (Bounded, Enum, Eq, Show)

render :: Tile -> Text
render = \case
  PlayerShip -> "S"
  EnemyShip -> "<"
  DestroyedEnemyShip -> "&"
  EmptySpace -> "."
  Star -> "*"
  Station -> "$"
