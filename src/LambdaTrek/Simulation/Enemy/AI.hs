module LambdaTrek.Simulation.Enemy.AI where

data EnemyState
  = Patrolling
  | Fighting
  deriving (Eq, Ord, Show)
