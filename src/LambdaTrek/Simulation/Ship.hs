{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Ship where

import Lens.Micro.TH

data Ship
  = Ship
  { shipPositionX :: Int
  , shipPositionY :: Int
  }
  deriving (Eq, Ord, Show)

makeFields ''Ship
