{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek where

import Brick (defaultMain)
import LambdaTrek.App
import LambdaTrek.State
import LambdaTrek.UI
import System.Random

run :: IO ()
run = do
  randGen <- initStdGen
  let initialState = mkForm $ initialGameState randGen
  _ <- defaultMain lambdaTrekApp initialState
  pure ()
