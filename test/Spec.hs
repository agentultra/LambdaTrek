{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import qualified Data.Array as Array
import Data.Functor.Identity
import LambdaTrek.Command
import LambdaTrek.Command.Parse
import LambdaTrek.Simulation
import LambdaTrek.Simulation.Combat
import LambdaTrek.Simulation.Dialog
import LambdaTrek.Simulation.Enemy as Enemy
import LambdaTrek.Simulation.Enemy.AI
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Ship as Ship
import LambdaTrek.Simulation.Tile
import LambdaTrek.State
import Test.Hspec
import System.Random
import Lens.Micro

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
        let initGameState = initialGameState gen
        ((`execState` initGameState) updateSimulation)
          `shouldBe`
          initGameState

      context "a valid EngineMove command" $ do
        it "should move the ship to the empty space" $ do
          let stateWithValidMoveCommand
                = (initialGameState gen)
                { _gameStateCommand = Just $ EngineMove 8 10
                }
              nextState = (`execState` stateWithValidMoveCommand) updateSimulation
          nextState^.(gameStateShip . Ship.positionX) `shouldBe` 8
          nextState^.(gameStateShip . Ship.positionY) `shouldBe` 10

      context "a Dock command" $ do
        it "should recharge the ship energy when next to a station" $ do
          let depletedShipState
                = (initialGameState gen)
                { _gameStateShip = Ship 8 1 0 6 10 ShieldsDown
                , _gameStateCommand = Just Dock
                }
              nextState = (`execState` depletedShipState) updateSimulation
          nextState^.(gameStateShip . energy) `shouldBe` 100
          nextState^.gameStateDialog `shouldSatisfy` hasDialog (Dialog Helm "Replenishing supplies at station (9, 1), sir!")
          nextState^.gameStateRemainingTurns `shouldBe` 185

        it "should not recharge the ship when not adjacent to a station" $ do
          let depletedShipState
                = (initialGameState gen)
                { _gameStateShip = Ship 0 0 0 6 10 ShieldsDown
                , _gameStateCommand = Just Dock
                }
              nextState = (`execState` depletedShipState) updateSimulation

          (nextState^.gameStateCommand) `shouldBe` Nothing
          (nextState^.gameStateDialog) `shouldSatisfy` hasDialog (Dialog Helm "There is no starbase to dock at nearby, captain.")

      context "When the player ship is in range of a enemy in Fighting state" $ do
        it "should damage the player ship" $ do
          let initialState
                = (initialGameState gen)
                { _gameStateShip = Ship 0 0 100 6 10 ShieldsDown
                , _gameStateCommand = Just $ EngineMove 7 3
                }
              nextState = (`execState` initialState) updateSimulation
              (Just enemy) = Array.elems (nextState^.gameStateSector.enemyShips) ^? ix 0
              initialStateShip = initialState^.gameStateShip
              nextStateShip = nextState^.gameStateShip
          enemy^.Enemy.state `shouldBe` Fighting
          nextStateShip^.hull `shouldSatisfy` (< initialStateShip^.hull)

  describe "LambdaTrek.Simulation.Combat" $ do
    describe "enemyInRange" $ do
      let exampleEnemy = Enemy 1 1 1 0 Patrolling
      it "should return True of the enemy is in range" $ do
        enemyInRange (0, 0) (3, 3) exampleEnemy `shouldBe` True

      it "should return False if the enemy is out of range" $ do
        enemyInRange (8, 8) (8, 8) exampleEnemy `shouldBe` False

    describe "enemiesInRange" $ do
      let exampleEnemies = Array.listArray (0, 1)
            [ Enemy 1 1 1 0 Patrolling
            , Enemy 4 4 1 0 Patrolling
            ]
      it "should return all enemies in the range" $
        enemiesInRange (0, 0) (8, 8) exampleEnemies
        `shouldBe`
        [(0, Enemy 1 1 1 0 Patrolling), (1, Enemy 4 4 1 0 Patrolling)]

      it "should return some enemies in the range" $
        enemiesInRange (0, 0) (2, 2) exampleEnemies `shouldBe` [(0, Enemy 1 1 1 0 Patrolling)]

      it "should not return destroyed enemies" $ do
        let exampleDestroyedEnemies = Array.listArray (0, 2)
              [ Enemy 1 1 1 0 Patrolling
              , Enemy 4 4 1 0 Patrolling
              , Enemy 2 2 0 0 Patrolling
              ]
        enemiesInRange (0, 0) (8, 8) exampleDestroyedEnemies
          `shouldBe`
          [(0, Enemy 1 1 1 0 Patrolling), (1, Enemy 4 4 1 0 Patrolling)]

    describe "calculateEnemyPhaserDamage" $ do
      let initState = initialGameState (mkStdGen 0)

      -- The `amt` parameter to 'calculateEnemyPhaserDamage' is a
      -- positive integer > 0 as defined by the PHASER command from
      -- the user and is restricted by the parser.
      -- TODO: consider making this clearer with a newtype

      context "When the enemy shields are down" $ do
        it "should apply some damage" $ do
          let e = Enemy
                  { enemyPositionX = 0
                  , enemyPositionY = 0
                  , enemyHitPoints = 10
                  , enemyShieldValue = 0
                  , enemyState = Patrolling
                  }
              Identity phaserDamageResult
                = (`evalStateT` initState)
                $ calculateEnemyPhaserDamage 5 (0, e)
              damageIsApplied (PhaserDamageResult damageDealt shieldDamage _ damagedE) =
                damageDealt /= 0
                && shieldDamage == 0
                && damagedE^.shieldValue == 0
                && damagedE^.hitPoints + damageDealt == e^.hitPoints
          phaserDamageResult `shouldSatisfy` damageIsApplied

      context "When the enemy shields are up" $ do
        it "should do some damage when the energy is greater than shields" $ do
          let e = Enemy
                { enemyPositionX = 0
                , enemyPositionY = 0
                , enemyHitPoints = 10
                , enemyShieldValue = 5
                , enemyState = Patrolling
                }
              Identity phaserDamageResult
                = (`evalStateT` initState)
                $ calculateEnemyPhaserDamage 10 (0, e)
              shieldDamage (PhaserDamageResult damageDealt shieldDamageDealt _ enemy) =
                shieldDamageDealt == 5
                && damageDealt > 0
                && enemy^.shieldValue == 0
                && enemy^.hitPoints < e^.hitPoints
          phaserDamageResult `shouldSatisfy` shieldDamage

        it "should prevent some damage" $ do
          let e1 = Enemy
                  { enemyPositionX = 0
                  , enemyPositionY = 0
                  , enemyHitPoints = 10
                  , enemyShieldValue = 0
                  , enemyState = Patrolling
                  }
              e2 = Enemy
                  { enemyPositionX = 0
                  , enemyPositionY = 0
                  , enemyHitPoints = 10
                  , enemyShieldValue = 8
                  , enemyState = Patrolling
                  }

              Identity (PhaserDamageResult _ _ _ damagedE1)
                = (`evalStateT` initState)
                $ calculateEnemyPhaserDamage 5 (0, e1)
              Identity (PhaserDamageResult _ _ _ damagedE2)
                = (`evalStateT` initState)
                $ calculateEnemyPhaserDamage 5 (0, e2)

              shieldsReducedDamage (enemy1, enemy2) =
                case compare (enemy1^.hitPoints) (enemy2^.hitPoints) of
                  LT -> True
                  _  -> False

          (damagedE1, damagedE2) `shouldSatisfy` shieldsReducedDamage

        it "should soak damage" $ do
          let e = Enemy
                { enemyPositionX = 0
                , enemyPositionY = 0
                , enemyHitPoints = 10
                , enemyShieldValue = 10
                , enemyState = Patrolling
                }

              Identity phaserDamageResult
                = (`evalStateT` initState)
                $ calculateEnemyPhaserDamage 5 (0, e)

              shieldsReduced (PhaserDamageResult damageDealt shieldDamage _ damagedEnemy) =
                damageDealt == 0
                && shieldDamage == 5
                && damagedEnemy^.shieldValue == 5

          phaserDamageResult `shouldSatisfy` shieldsReduced

        it "should drop shields" $ do
          let e = Enemy
                { enemyPositionX = 0
                , enemyPositionY = 0
                , enemyHitPoints = 10
                , enemyShieldValue = 10
                , enemyState = Patrolling
                }

              Identity phaserDamageResult
                = (`evalStateT` initState)
                $ calculateEnemyPhaserDamage 10 (0, e)

              shieldsReduced (PhaserDamageResult damageDealt shieldDamage _ damagedEnemy) =
                damageDealt == 0
                && shieldDamage == 10
                && damagedEnemy^.shieldValue == 0
                && damagedEnemy^.hitPoints == e^.hitPoints

          phaserDamageResult `shouldSatisfy` shieldsReduced

  describe "LambdaTrek.Simulation.Enemy.AI" $ do
    let gen = mkStdGen 0
    context "When in the Patrolling state" $ do
      it "should do nothing when the player ship is out of range" $ do
        let initialState
              = (initialGameState gen)
              { _gameStateShip = Ship 0 0 6 100 10 ShieldsDown
              , _gameStateCommand = Just (EngineMove 14 14)
              }
            nextState = (`execState` initialState) updateSimulation
            (Just enemy) = Array.elems (nextState^.gameStateSector.enemyShips) ^? ix 0
        enemy^.Enemy.state `shouldBe` Patrolling

      it "should do nothing if the enemy is destroyed" $ do
        let initialState
              = (initialGameState gen)
              { _gameStateShip = Ship 0 0 6 100 10 ShieldsDown
              , _gameStateSector = emptySector { sectorEnemyShips = Array.listArray (0,0) [Enemy 8 3 0 10 Patrolling] }
              , _gameStateCommand = Just (EngineMove 7 3)
              }
            nextState = (`execState` initialState) updateSimulation
            (Just enemy) = Array.elems (nextState^.gameStateSector.enemyShips) ^? ix 0
        enemy^.Enemy.state `shouldBe` Patrolling

      it "should change to Fighting when the player ship is in range" $ do
        let initialState
              = (initialGameState gen)
              { _gameStateShip = Ship 14 14 6 100 10 ShieldsDown
              , _gameStateCommand = Just (EngineMove 7 3)
              }
            nextState = (`execState` initialState) updateSimulation
            (Just enemy) = Array.elems (nextState^.gameStateSector.enemyShips) ^? ix 0
        enemy^.Enemy.state `shouldBe` Fighting

    context "When in the Fighting state" $ do
      it "should transition to patrolling when the player moves out of range" $ do
        let initialState
              = (initialGameState gen)
              { _gameStateShip = Ship 14 14 6 100 10 ShieldsDown
              , _gameStateCommand = Just (EngineMove 7 3)
              }
            nextState = (`execState` initialState) updateSimulation
            (Just enemy) = Array.elems (nextState^.gameStateSector.enemyShips) ^? ix 0
        enemy^.Enemy.state `shouldBe` Fighting

        let finalState = (`execState` nextState { _gameStateCommand = Just (EngineMove 14 14) }) updateSimulation

            (Just enemy') = Array.elems (finalState^.gameStateSector.enemyShips) ^? ix 0

        enemy'^.Enemy.state `shouldBe` Patrolling

hasDialog :: Dialog -> [Dialog] -> Bool
hasDialog = elem
