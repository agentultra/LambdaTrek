{-# LANGUAGE FlexibleInstances #-}

module LambdaTrek.Simulation.Position where

class HasPosition a where
  getPosition :: a -> (Int, Int)

instance HasPosition (Int, Int) where
  getPosition (x, y) = (x, y)
