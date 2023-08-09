{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import qualified Data.Array as Array
import Data.Functor.Identity
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
      let exampleEnemy = Enemy 1 1 1 0
      it "should return True of the enemy is in range" $ do
        enemyInRange (0, 0) (3, 3) exampleEnemy `shouldBe` True

      it "should return False if the enemy is out of range" $ do
        enemyInRange (8, 8) (8, 8) exampleEnemy `shouldBe` False

    describe "enemiesInRange" $ do
      let exampleEnemies = Array.listArray (0, 1)
            [ Enemy 1 1 1 0
            , Enemy 4 4 1 0
            ]
      it "should return all enemies in the range" $
        enemiesInRange (0, 0) (8, 8) exampleEnemies
        `shouldBe`
        [(0, Enemy 1 1 1 0), (1, Enemy 4 4 1 0)]

      it "should return some enemies in the range" $
        enemiesInRange (0, 0) (2, 2) exampleEnemies `shouldBe` [(0, Enemy 1 1 1 0)]

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
                  }
              e2 = Enemy
                  { enemyPositionX = 0
                  , enemyPositionY = 0
                  , enemyHitPoints = 10
                  , enemyShieldValue = 8
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
