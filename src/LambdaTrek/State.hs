module LambdaTrek.State where

newtype GameState
  = GameState
  { _gameStateDisplayMessage :: String
  }
  deriving (Eq, Ord, Show)
