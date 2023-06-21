{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek where

import Brick (defaultMain)
import LambdaTrek.App
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Ship
import LambdaTrek.State
import LambdaTrek.UI

run :: IO ()
run = do
  let initialState = mkForm $ GameState "" Nothing emptySector (Ship 2 2)
  _ <- defaultMain lambdaTrekApp initialState
  pure ()
