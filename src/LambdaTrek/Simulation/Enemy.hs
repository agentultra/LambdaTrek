{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Enemy where

import Lens.Micro
import Lens.Micro.TH

data Enemy
  = Enemy
  { enemyPositionX :: Int
  , enemyPositionY :: Int
  , enemyHitPoints :: Int
  }
  deriving (Eq, Ord, Show)

makeFields ''Enemy

damageEnemy :: Int -> Enemy -> Enemy
damageEnemy amt enemy = enemy & hitPoints %~ \oldAmt -> oldAmt - amt
