{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek where

import Brick (defaultMain)
import LambdaTrek.App
import LambdaTrek.State
import LambdaTrek.UI

run :: IO ()
run = do
  let initialState = mkForm initialGameState
  _ <- defaultMain lambdaTrekApp initialState
  pure ()
