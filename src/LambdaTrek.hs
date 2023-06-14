module LambdaTrek where

import Brick (defaultMain)
import LambdaTrek.App
import LambdaTrek.State

run :: IO ()
run = do
  let initialState = GameState "HELLO WORLD"
  _ <- defaultMain lambdaTrekApp initialState
  pure ()
