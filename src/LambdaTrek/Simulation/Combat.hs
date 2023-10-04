{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaTrek.Simulation.Combat where

import Control.Monad.State
import Data.Array
import qualified Data.Array as Array
import Data.Text (Text)
import qualified Data.Text as T
import LambdaTrek.Command
import LambdaTrek.List (allp)
import LambdaTrek.State
import LambdaTrek.Simulation.Dialog
import LambdaTrek.Simulation.Enemy (Enemy (..))
import qualified LambdaTrek.Simulation.Enemy as Enemy
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Ship (Ship (..))
import qualified LambdaTrek.Simulation.Ship as Ship
import Lens.Micro
import Lens.Micro.Mtl
import System.Random

handleFirePhasers :: Int -> PhaserMode -> State GameState CommandResult
handleFirePhasers energyAmt firingMode = do
  enemies <- getEnemiesInPhaserRange
  playerShip <- use gameStateShip
  case compare (playerShip^.Ship.energy) energyAmt of
    LT -> do
      sayDialog Combat "We don't have that much energy to fire the phasers, sir!"
      pure Denied
    _ -> case playerShip^.Ship.shieldState of
      Ship.ShieldsUp -> do
        sayDialog Combat
          $ "Shields are up captain, we must first lower "
          <> "the shields in order to fire phasers."
        pure Denied
      Ship.ShieldsDown -> doFire energyAmt enemies firingMode

doFire :: Int -> [(Int, Enemy)] -> PhaserMode -> State GameState CommandResult
doFire energyAmt enemies PhaserAutomatic = do
  let energyAmount = energyAmt `div` length enemies
  damagedEnemies <- mapM (calculateEnemyPhaserDamage energyAmount) enemies
  gameStateShip %= Ship.subtractEnergy (energyAmount * length damagedEnemies)
  gameStateSector . enemyShips %= \ships ->
    ships Array.// map resultToEnemyIx damagedEnemies
  x <- forM damagedEnemies $ \PhaserDamageResult {..} ->
    if Enemy.isDestroyed _phaserDamageReportEnemy
    then sayDialog Combat "Enemy ship destroyed, captain!"
    else sayDialog Combat (generateDamageDialog _phaserDamageReportHitPointDamage _phaserDamageReportEnemy)
  if null x
    then pure Performed
    else pure Denied

doFire _ _ PhaserManual = pure Denied

data PhaserDamageResult
  = PhaserDamageResult
  { _phaserDamageReportHitPointDamage :: Int
  , _phaserDamageReportShieldValueDamage :: Int
  , _phaserDamageReportEnemyIdx :: Int
  , _phaserDamageReportEnemy :: Enemy
  }
  deriving (Eq, Show)

resultToEnemyIx :: PhaserDamageResult -> (Int, Enemy)
resultToEnemyIx PhaserDamageResult {..} =
  (_phaserDamageReportEnemyIdx, _phaserDamageReportEnemy)

calculateEnemyPhaserDamage
  :: Int
  -> (Int, Enemy)
  -> State GameState PhaserDamageResult
calculateEnemyPhaserDamage amt (idx, enemy) = do
  factor <- randomPhaserFactor
  let shieldDmg | amt <= enemy^.Enemy.shieldValue = amt
                | amt > enemy^.Enemy.shieldValue = enemy^.Enemy.shieldValue
                | otherwise = 0
      dmgAmount | amt - enemy^.Enemy.shieldValue > 0 = (amt - shieldDmg) + ceiling factor
                | otherwise = 0
  pure $ PhaserDamageResult
    { _phaserDamageReportHitPointDamage = dmgAmount
    , _phaserDamageReportShieldValueDamage = shieldDmg
    , _phaserDamageReportEnemyIdx = idx
    , _phaserDamageReportEnemy = Enemy.applyDamageToShields shieldDmg
                               . Enemy.applyDamage dmgAmount
                               $ enemy
    }

getEnemiesInPhaserRange :: State GameState [(Int, Enemy)]
getEnemiesInPhaserRange = do
  Ship {..} <- use gameStateShip
  sector <- use gameStateSector
  let rangeBoxCorner = ( max 0 (shipPositionX - (shipPhaserRange `div` 2))
                       , max 0 (shipPositionY - (shipPhaserRange `div` 2)))
      rangeBoxOffset = (shipPhaserRange, shipPhaserRange)
  pure . enemiesInRange rangeBoxCorner rangeBoxOffset $ sector^.enemyShips

handleFireTorpedo :: Int -> [(Int, Int)] -> State GameState CommandResult
handleFireTorpedo num coords = do
  ship <- use gameStateShip
  sector <- use gameStateSector
  case compare num (ship^.Ship.torpedos) of
    GT -> do
      sayDialog Combat
        $ "Cannot fire torpedos! We only have "
        <> T.pack (show $ ship^.Ship.torpedos)
        <> " torpedos left, captain."
      pure Denied
    _ -> do
      sayDialog Combat "Aye, firing torpedos"
      forM_ coords $ \coord -> do
        case enemyAtCoord coord sector of
          Nothing -> do
            sayDialog Combat
              $ "Torpedo detonated at "
              <> T.pack (show coord)
              <> " in empty space!"
          Just (enemyIx, enemy) -> do
            sayDialog Combat "Enemy hit, sir!"
            let damagedEnemy = Enemy.destroyEnemy enemy
            zoom gameStateSector $
              enemyShips %= \ships -> ships Array.// [(enemyIx, damagedEnemy)]
      zoom gameStateShip $
        Ship.torpedos %= \currentAmt -> currentAmt - num
      pure Performed

enemyInRange :: (Int, Int) -> (Int, Int) -> Enemy -> Bool
enemyInRange (topLeftX, topLeftY) (offSetX, offSetY) Enemy {..} =
  enemyPositionX >= topLeftX
  && (topLeftX + offSetX) >= enemyPositionX
  && enemyPositionY >= topLeftY
  && (topLeftY + offSetY) >= enemyPositionY

enemiesInRange :: (Int, Int) -> (Int, Int) -> Array Int Enemy -> [(Int, Enemy)]
enemiesInRange rangeBoxCorner rangeBoxOffset
  = filter (validEnemy . snd)
  . Array.assocs
  where
    validEnemy
      = allp
      [ not . Enemy.isDestroyed
      , enemyInRange rangeBoxCorner rangeBoxOffset
      ]

generateDamageDialog :: Int -> Enemy -> Text
generateDamageDialog amt _
  | amt <= 0  = "Weapons did not make contact, sir!"
  | amt < 20  = "Minimal damage, sir."
  | amt < 30  = "We hit the enemy ship, sir."
  | amt < 50  = "A direct hit, captain!"
  | otherwise = "The enemy has taken a heavy blow, captain!"

randomPhaserFactor :: State GameState Float
randomPhaserFactor = do
  gen <- use gameStateRandomGen
  let (f, gen') = randomR (0.9, 1.1) gen
  gameStateRandomGen .= gen'
  pure f

randomRange :: Int -> Int -> State GameState Int
randomRange lo hi = do
  gen <- use gameStateRandomGen
  let (x, gen') = randomR (lo, hi) gen
  gameStateRandomGen .= gen'
  pure x
