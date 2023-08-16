{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Enemy where

import Lens.Micro
import Lens.Micro.TH

data Enemy
  = Enemy
  { enemyPositionX   :: Int
  , enemyPositionY   :: Int
  , enemyHitPoints   :: Int
  , enemyShieldValue :: Int
  }
  deriving (Eq, Ord, Show)

makeFields ''Enemy

isDestroyed :: Enemy -> Bool
isDestroyed Enemy {..} = enemyHitPoints <= 0

applyDamage :: Int -> Enemy -> Enemy
applyDamage dmg enemy = enemy & hitPoints %~ \hp -> hp - dmg

applyDamageToShields :: Int -> Enemy -> Enemy
applyDamageToShields dmg enemy = enemy & shieldValue %~ \sv -> max 0 (sv - dmg)
