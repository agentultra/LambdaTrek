{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek.Simulation.Tile where

import Data.Text (Text)

data Tile
  = PlayerShip
  | EmptySpace
  deriving (Bounded, Enum, Eq, Show)

render :: Tile -> Text
render = \case
  PlayerShip -> "S"
  EmptySpace -> "."
