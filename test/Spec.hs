{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Array as Array
import LambdaTrek.Command
import LambdaTrek.Command.Parse
import LambdaTrek.Simulation
import LambdaTrek.Simulation.Combat
import LambdaTrek.Simulation.Enemy
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Ship
import LambdaTrek.Simulation.Tile
import LambdaTrek.State
import Test.Hspec
import System.Random

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
      let gen = mkStdGen 0
      it "is basically the id function when there is no command" $ do
        let expectedState = initialGameState gen
        updateSimulation expectedState `shouldBe` expectedState

      context "a valid EngineMove command" $ do
        it "should move the ship to the empty space" $ do
          let stateWithValidMoveCommand
                = (initialGameState gen)
                { _gameStateCommand = Just $ EngineMove 8 10
                }
              expectedState
                = (initialGameState gen)
                { _gameStateCommand = Nothing
                , _gameStateShip = Ship 8 10 98 6
                }
          updateSimulation stateWithValidMoveCommand `shouldBe` expectedState

  describe "LambdaTrek.Simulation.Combat" $ do
    describe "enemyInRange" $ do
      let exampleEnemy = Enemy 1 1 1
      it "should return True of the enemy is in range" $ do
        enemyInRange (0, 0) (3, 3) exampleEnemy `shouldBe` True

      it "should return False if the enemy is out of range" $ do
        enemyInRange (8, 8) (8, 8) exampleEnemy `shouldBe` False

    describe "enemiesInRange" $ do
      let exampleEnemies = Array.listArray (0, 1)
            [ Enemy 1 1 1
            , Enemy 4 4 1
            ]
      it "should return all enemies in the range" $
        enemiesInRange (0, 0) (8, 8) exampleEnemies
        `shouldBe`
        [(0, Enemy 1 1 1), (1, Enemy 4 4 1)]

      it "should return some enemies in the range" $
        enemiesInRange (0, 0) (2, 2) exampleEnemies `shouldBe` [(0, Enemy 1 1 1)]
