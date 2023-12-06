{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Enemy where

import LambdaTrek.Simulation.Enemy.AI
import LambdaTrek.Simulation.Position
import Lens.Micro
import Lens.Micro.TH

data Enemy
  = Enemy
  { enemyPositionX        :: Int
  , enemyPositionY        :: Int
  , enemyHitPoints        :: Int
  , enemyShieldValue      :: Int
  , enemyState            :: EnemyState
  , enemyBaseDamageAmount :: Int
  }
  deriving (Eq, Ord, Show)

instance HasPosition Enemy where
  getPosition Enemy {..} = (enemyPositionX, enemyPositionY)

makeFields ''Enemy

enemyRange :: Int
enemyRange = 3

isDestroyed :: Enemy -> Bool
isDestroyed Enemy {..} = enemyHitPoints <= 0

isAlive :: Enemy -> Bool
isAlive = not . isDestroyed

applyDamage :: Int -> Enemy -> Enemy
applyDamage dmg enemy = enemy & hitPoints %~ \hp -> hp - dmg

applyDamageToShields :: Int -> Enemy -> Enemy
applyDamageToShields dmg enemy = enemy & shieldValue %~ \sv -> max 0 (sv - dmg)

destroyEnemy :: Enemy -> Enemy
destroyEnemy enemy = enemy { enemyHitPoints = 0 }
