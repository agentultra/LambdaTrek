module LambdaTrek.Simulation.Dialog where

import Data.Text (Text)

data Crewmate
  = Helm
  deriving (Eq, Ord, Show)

data Dialog = Dialog Crewmate Text
  deriving (Eq, Ord, Show)
