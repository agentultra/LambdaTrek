{-# LANGUAGE OverloadedStrings #-}

import LambdaTrek.Command
import LambdaTrek.Command.Parse
import LambdaTrek.Simulation
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Ship
import LambdaTrek.Simulation.Tile
import LambdaTrek.State
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "LambdaTrek.Command.Parse" $ do
    it "should parse MOV command" $ do
      runCommandParser "MOV 2 2" `shouldBe` (Right $ EngineMove 2 2)

    it "should not parse invalid MOV command" $ do
      runCommandParser "MOV 100 100"
        `shouldBe`
        (Left $ InvalidEngineMove "Invalid move: X and Y must be from 0 to 14")

  describe "LambdaTrek.Simulation" $ do
    describe "LambdaTrek.Simulation.Sector" $ do
      describe "getTile" $ do
        it "should get the tile at 0, 0" $ do
          getTile 0 0 emptySectorTiles `shouldBe` Just EmptySpace

        it "should get the tile at 14, 14" $ do
          getTile 14 14 emptySectorTiles `shouldBe` Just EmptySpace

        it "should get Nothing for something out of bounds" $ do
          getTile 200 200 emptySectorTiles `shouldBe` Nothing

    describe "updateSimulation" $ do
      it "is basically the id function when there is no command" $ do
        updateSimulation initialGameState `shouldBe` initialGameState

      context "a valid EngineMove command" $ do
        it "should move the ship to the empty space" $ do
          let stateWithValidMoveCommand
                = initialGameState
                { _gameStateCommand = Just $ EngineMove 10 10
                }
              expectedState
                = initialGameState
                { _gameStateCommand = Nothing
                , _gameStateShip = Ship 10 10
                }
          updateSimulation stateWithValidMoveCommand `shouldBe` expectedState
