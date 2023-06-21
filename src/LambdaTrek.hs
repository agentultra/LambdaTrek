{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek where

import Brick (defaultMain)
import LambdaTrek.App
import LambdaTrek.Simulation.Sector
import LambdaTrek.State
import LambdaTrek.UI

run :: IO ()
run = do
  let initialState = mkForm $ GameState "" Nothing emptySector
  _ <- defaultMain lambdaTrekApp initialState
  pure ()
