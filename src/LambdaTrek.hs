{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek where

import Brick (defaultMain)
import qualified Data.Text as Text
import LambdaTrek.App
import LambdaTrek.CLI
import LambdaTrek.Config
import LambdaTrek.State
import LambdaTrek.UI
import Options.Applicative
import System.Random

run :: IO ()
run = do
  gameConfig <- execParser $ info cliParser briefDesc
  case validConfig gameConfig of
    Left err -> do
      putStrLn $ Text.unpack err
    Right config -> do
      randGen <- initStdGen
      let initialState = mkForm $ initialGameState config randGen
      _ <- defaultMain lambdaTrekApp initialState
      pure ()
