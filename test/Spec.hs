{-# OPTIONS -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.State
import qualified Data.Array as Array
import Data.Either
import Data.Functor.Identity
import qualified Data.Map as M
import LambdaTrek.Command
import LambdaTrek.Command.Parse
import LambdaTrek.Config
import LambdaTrek.Simulation
import LambdaTrek.Simulation.Combat
import LambdaTrek.Simulation.Dialog
import LambdaTrek.Simulation.Enemy as Enemy
import LambdaTrek.Simulation.Enemy.AI
import LambdaTrek.Simulation.Quadrant hiding (getTile)
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Sector.Internal
import LambdaTrek.Simulation.Ship as Ship
import qualified LambdaTrek.Simulation.Station as Station
import LambdaTrek.Simulation.Tile
import LambdaTrek.State
import Test.Hspec
import System.Random
import Lens.Micro

initialTestGameState :: StdGen -> GameState
initialTestGameState gen =
  GameState
  { _gameStateCommandInput = ""
  , _gameStateCommand = Nothing
  , _gameStateCommandError = Nothing
  , _gameStateQuadrant = initTestQuadrant (0, 0)
  , _gameStateSector = (0, 0)
  , _gameStateShip = Ship 2 2 100 6 30 ShieldsDown 0.75 5 WarpFactorOne
  , _gameStateRemainingTurns = 200
  , _gameStateDialog = []
  , _gameStateRandomGen = gen
  , _gameStateScreen = SectorScreen
  , _gameStateGameConfig = defaultConfig
  }

initTestQuadrant :: (Int, Int) -> Quadrant
initTestQuadrant startingCoord =
  let sectorStarMap
        = M.fromList
        [ (coord, [(10, 10)]) | coord <- quadrantCoords ]
      sectorEnemyShipMap
        = M.fromList
        [ (coord, if coord == (0, 0) then [Enemy 8 3 20 10 Patrolling 10] else [])
        | coord <- quadrantCoords
        ]
      sectorStationMap
        = M.fromList
        [ (coord, [Station.Station 9 1 100])
        | coord <- quadrantCoords
        ]
  in Quadrant
     { _quadrantStars = sectorStarMap
     , _quadrantEnemyShips = sectorEnemyShipMap
     , _quadrantStations = sectorStationMap
     , _quadrantScanState = M.singleton startingCoord True
     }

main :: IO ()
main = hspec $ do
  describe "LambdaTrek.Command.Parse" $ do
    it "should parse MOV command" $ do
      runCommandParser "MOV 2 2" `shouldBe` (Right $ EngineMove 2 2)

    it "should not parse invalid MOV command" $ do
      runCommandParser "MOV 100 100"
        `shouldBe`
        (Left $ InvalidEngineMove "Invalid move: X and Y must be from 0 to 14")

    it "should parse TORPEDO command with 1 target" $ do
      runCommandParser "TORPEDO 1 1 1"
        `shouldSatisfy`
        isRight

    it "should parse TORPEDO command with 2 targets" $ do
      runCommandParser "TORPEDO 2 1 1 2 2"
        `shouldBe`
        (Right $ FireTorpedo 2 [(1, 1), (2, 2)])

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
        let initGameState = initialTestGameState gen
        ((`execState` initGameState) updateSimulation)
          `shouldBe`
          initGameState

      context "a valid EngineMove command" $ do
        it "should move the ship to the empty space" $ do
          let stateWithValidMoveCommand
                = (initialTestGameState gen)
                { _gameStateCommand = Just $ EngineMove 8 10
                }
              nextState = (`execState` stateWithValidMoveCommand) updateSimulation
          nextState^.(gameStateShip . Ship.positionX) `shouldBe` 8
          nextState^.(gameStateShip . Ship.positionY) `shouldBe` 10

      context "a Dock command" $ do
        it "should recharge the ship energy when next to a station" $ do
          let depletedShipState
                = (initialTestGameState gen)
                { _gameStateShip = Ship 8 1 0 6 10 ShieldsDown 0.75 5 WarpFactorOne
                , _gameStateCommand = Just Dock
                , _gameStateQuadrant = initTestQuadrant (8, 1)
                }
              nextState = (`execState` depletedShipState) updateSimulation
          nextState^.(gameStateShip . energy) `shouldBe` 100
          nextState^.gameStateDialog `shouldSatisfy` hasDialog (Dialog Helm "Replenishing supplies at station (9, 1), sir!")
          nextState^.gameStateRemainingTurns `shouldBe` 185

        it "should not recharge the ship when not adjacent to a station" $ do
          let depletedShipState
                = (initialTestGameState gen)
                { _gameStateShip = Ship 0 0 0 6 10 ShieldsDown 0.75 5 WarpFactorOne
                , _gameStateCommand = Just Dock
                , _gameStateQuadrant = initTestQuadrant (0, 0)
                }
              nextState = (`execState` depletedShipState) updateSimulation

          (nextState^.gameStateCommand) `shouldBe` Nothing
          (nextState^.gameStateDialog) `shouldSatisfy` hasDialog (Dialog Helm "There is no starbase to dock at nearby, captain.")

      context "When the player ship is in range of a enemy in Fighting state" $ do
        it "should damage the player ship" $ do
          let initialState
                = (initialTestGameState gen)
                { _gameStateShip = Ship 0 0 100 6 10 ShieldsDown 0.75 5 WarpFactorOne
                , _gameStateCommand = Just $ EngineMove 7 3
                , _gameStateQuadrant = initTestQuadrant (0, 0)
                }
              nextState = (`execState` initialState) updateSimulation
              nextSector = getSector (nextState^.gameStateQuadrant) (nextState^.gameStateSector)
              (Just enemy) = Array.elems (nextSector^.enemyShips) ^? ix 0
              initialStateShip = initialState^.gameStateShip
              nextStateShip = nextState^.gameStateShip
          enemy^.Enemy.state `shouldBe` Fighting
          nextStateShip^.hull `shouldSatisfy` (< initialStateShip^.hull)

  describe "LambdaTrek.Simulation.Combat" $ do
    describe "enemyInRange" $ do
      let exampleEnemy = Enemy 1 1 1 0 Patrolling 10
      it "should return True of the enemy is in range" $ do
        enemyInRange (0, 0) (3, 3) exampleEnemy `shouldBe` True

      it "should return False if the enemy is out of range" $ do
        enemyInRange (8, 8) (8, 8) exampleEnemy `shouldBe` False

    describe "enemiesInRange" $ do
      let exampleEnemies = Array.listArray (0, 1)
            [ Enemy 1 1 1 0 Patrolling 10
            , Enemy 4 4 1 0 Patrolling 10
            ]
      it "should return all enemies in the range" $
        enemiesInRange (0, 0) (8, 8) exampleEnemies
        `shouldBe`
        [(0, Enemy 1 1 1 0 Patrolling 10), (1, Enemy 4 4 1 0 Patrolling 10)]

      it "should return some enemies in the range" $
        enemiesInRange (0, 0) (2, 2) exampleEnemies `shouldBe` [(0, Enemy 1 1 1 0 Patrolling 10)]

      it "should not return destroyed enemies" $ do
        let exampleDestroyedEnemies = Array.listArray (0, 2)
              [ Enemy 1 1 1 0 Patrolling 10
              , Enemy 4 4 1 0 Patrolling 10
              , Enemy 2 2 0 0 Patrolling 10
              ]
        enemiesInRange (0, 0) (8, 8) exampleDestroyedEnemies
          `shouldBe`
          [(0, Enemy 1 1 1 0 Patrolling 10), (1, Enemy 4 4 1 0 Patrolling 10)]

    describe "calculateEnemyPhaserDamage" $ do
      let initState = initialTestGameState (mkStdGen 0)

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
                  , enemyBaseDamageAmount = 10
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
                , enemyBaseDamageAmount = 10
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
                  , enemyBaseDamageAmount = 10
                  }
              e2 = Enemy
                  { enemyPositionX = 0
                  , enemyPositionY = 0
                  , enemyHitPoints = 10
                  , enemyShieldValue = 8
                  , enemyState = Patrolling
                  , enemyBaseDamageAmount = 10
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
                , enemyBaseDamageAmount = 10
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
                , enemyBaseDamageAmount = 10
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
              = (initialTestGameState gen)
              { _gameStateShip = Ship 0 0 6 100 10 ShieldsDown 0.75 5 WarpFactorOne
              , _gameStateCommand = Just (EngineMove 14 14)
              , _gameStateQuadrant = initTestQuadrant (0, 0)
              }
            nextState = (`execState` initialState) updateSimulation
            nextSector = getSector (nextState^.gameStateQuadrant) (nextState^.gameStateSector)
            (Just enemy) = Array.elems (nextSector^.enemyShips) ^? ix 0
        enemy^.(Enemy.state) `shouldBe` Patrolling

      it "should do nothing if the enemy is destroyed" $ do
        -- emptySector { sectorEnemyShips =  }
        let initialState
              = (initialTestGameState gen)
              { _gameStateShip = Ship 0 0 6 100 10 ShieldsDown 0.75 5 WarpFactorOne
              , _gameStateQuadrant = (initTestQuadrant (0, 0)) { _quadrantEnemyShips = M.singleton (0, 0) [Enemy 8 3 0 10 Patrolling 10] }
              , _gameStateSector = (0, 0)
              , _gameStateCommand = Just (EngineMove 7 3)
              }
            nextState = (`execState` initialState) updateSimulation
            nextSector = getSector (nextState^.gameStateQuadrant) (nextState^.gameStateSector)
            (Just enemy) = Array.elems (nextSector^.enemyShips) ^? ix 0
        enemy^.(Enemy.state) `shouldBe` Patrolling

      it "should change to Fighting when the player ship is in range" $ do
        let initialState
              = (initialTestGameState gen)
              { _gameStateShip = Ship 14 14 6 100 10 ShieldsDown 0.75 5 WarpFactorOne
              , _gameStateCommand = Just (EngineMove 7 3)
              , _gameStateQuadrant = initTestQuadrant (14, 14)
              }
            nextState = (`execState` initialState) updateSimulation
            nextSector = getSector (nextState^.gameStateQuadrant) (nextState^.gameStateSector)
            (Just enemy) = Array.elems (nextSector^.enemyShips) ^? ix 0
        enemy^.Enemy.state `shouldBe` Fighting

    context "When in the Fighting state" $ do
      it "should transition to patrolling when the player moves out of range" $ do
        let initialState
              = (initialTestGameState gen)
              { _gameStateShip = Ship 14 14 6 100 10 ShieldsDown 0.75 5 WarpFactorOne
              , _gameStateCommand = Just (EngineMove 7 3)
              , _gameStateQuadrant = initTestQuadrant (14, 14)
              }
            nextState = (`execState` initialState) updateSimulation
            nextSector = getSector (nextState^.gameStateQuadrant) (nextState^.gameStateSector)
            (Just enemy) = Array.elems (nextSector^.enemyShips) ^? ix 0
        enemy^.Enemy.state `shouldBe` Fighting

        let finalState = (`execState` nextState { _gameStateCommand = Just (EngineMove 14 14) }) updateSimulation
            finalSector = getSector (finalState^.gameStateQuadrant) (finalState^.gameStateSector)

            (Just enemy') = Array.elems (finalSector^.enemyShips) ^? ix 0

        enemy'^.Enemy.state `shouldBe` Patrolling

  describe "Sector" $ do
    describe "sector" $ do
      context "Given just enough stars" $ do
        it "should construct a Sector" $ do
          let notAllStars = [ (x, y)
                      | x <- [0..14], y <- [0..14]
                      , not (x == 14 && y == 14)
                      ]
          sector notAllStars [] [] `shouldSatisfy` isRight

      context "Given almost all stars and one enemy" $ do
        it "should not construct a Sector" $ do
          let notAllStars = [ (x, y)
                      | x <- [0..14], y <- [0..14]
                      , not (x == 14 && y == 14)
                      ]
              enemy = Enemy
                { enemyPositionX = 0
                , enemyPositionY = 0
                , enemyHitPoints = 10
                , enemyShieldValue = 10
                , enemyState = Patrolling
                , enemyBaseDamageAmount = 10
                }
          sector notAllStars [enemy] [] `shouldSatisfy` isLeft

      context "Given almost all stars, an enemy, and a station" $ do
        it "should not construct a Sector" $ do
          let notAllStars = [ (x, y)
                      | x <- [0..14], y <- [0..14]
                      , not (x == 14 && y == 14)
                      , not (x == 13 && y == 13)
                      ]
              enemy = Enemy
                      { enemyPositionX = 0
                      , enemyPositionY = 0
                      , enemyHitPoints = 10
                      , enemyShieldValue = 10
                      , enemyState = Patrolling
                      , enemyBaseDamageAmount = 10
                      }
              station = Station.Station 13 13 100
          sector notAllStars [enemy] [station] `shouldSatisfy` isLeft

      context "Given enough space for everyone" $ do
        it "should construct a Sector" $ do
          let notAllStars = [ (x, y)
                            | x <- [0..14], y <- [0..14]
                            , not (x == 14 && y == 14)
                            , not (x == 13 && y == 13)
                            ]
              enemy = Enemy
                      { enemyPositionX = 0
                      , enemyPositionY = 0
                      , enemyHitPoints = 10
                      , enemyShieldValue = 10
                      , enemyState = Patrolling
                      , enemyBaseDamageAmount = 10
                      }
          sector notAllStars [enemy] [] `shouldSatisfy` isRight

      context "Given all of the stars are at the same location" $ do
        it "should not construct a Sector" $ do
          let badAllStars = repeat (0, 0)
          sector badAllStars [] [] `shouldBe` Left "Duplicate stars"

    describe "findEmpty" $ do
      context "Given a nearly full sector" $ do
        let fullSector
              = Sector
              { sectorStars = [ (x, y)
                              | x <- [0..14], y <- [0..14]
                              , not (x == 14 && y == 14)
                              ]
              , sectorEnemyShips  = Array.listArray (1, 0) []
              , sectorStations = Array.listArray (1, 0) []
              }
        it "should give us the empty space" $ do
          let ship
                = Ship
                { shipPositionX = 0
                , shipPositionY = 0
                , shipEnergy = 100
                , shipPhaserRange = 0
                , shipHull = 0
                , shipShieldState = ShieldsDown
                , shipShieldStrength = 0
                , shipTorpedos = 1
                , shipWarpFactor = WarpFactorOne
                }
              sectorTiles = buildSectorTiles fullSector
              -- TODO: add ship back in here
          findEmpty sectorTiles `shouldBe` (14, 14)

    describe "Quadrant" $ do
      describe "availableSpace" $ do
        context "Given a mostly empty sector" $ do
          it "should be True" $ do
            availableSpace (initTestQuadrant (0, 0)) (0, 0) `shouldBe` True

        context "Given a sector with only 1 empty space" $ do
          it "should be False" $ do
            let quadrantWithMostlyFullSector = testFillSector (1, 1) $ initTestQuadrant (0, 0)
            availableSpace quadrantWithMostlyFullSector (1, 1) `shouldBe` False
            where
              testFillSector :: (Int, Int) -> Quadrant -> Quadrant
              testFillSector sectorCoord quadrant =
                let starCoords = [(x, y) | x <- [0..14], y <- [0..14]]
                in go starCoords sectorCoord quadrant

              go :: [(Int, Int)] -> (Int, Int) -> Quadrant -> Quadrant
              go [] _ q = q
              go (starCoord:ss) sc q =
                case addStar sc starCoord q of
                  Nothing -> q
                  Just q' -> go ss sc q'

hasDialog :: Dialog -> [Dialog] -> Bool
hasDialog = elem
