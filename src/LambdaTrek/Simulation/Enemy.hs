{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Enemy where

import Lens.Micro.TH

data Enemy
  = Enemy
  { enemyPositionX :: Int
  , enemyPositionY :: Int
  }
  deriving (Eq, Ord, Show)

makeFields ''Enemy
