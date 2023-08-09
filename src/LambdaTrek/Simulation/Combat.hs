{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaTrek.Simulation.Combat where

import Control.Monad.State
import Data.Array
import qualified Data.Array as Array
import Data.Text (Text)
import qualified Data.Text as T
import LambdaTrek.Command
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

handleFirePhasers :: Int -> PhaserMode -> State GameState ()
handleFirePhasers energyAmt firingMode = do
  enemies <- getEnemiesInPhaserRange
  playerShip <- use gameStateShip
  case compare (playerShip^.Ship.energy) energyAmt of
    LT -> do
      sayDialog Combat "We don't have that much energy to fire the phasers, sir!"
      pure ()
    _ -> do
      unless (null enemies) $
        case firingMode of
          PhaserAutomatic -> do
            let energyAmount = energyAmt `div` length enemies
            damagedEnemies <- mapM (calculateEnemyPhaserDamage energyAmount) enemies
            gameStateShip %= Ship.subtractEnergy (energyAmount * length damagedEnemies)
            gameStateSector . enemyShips %= \ships ->
              ships Array.// map resultToEnemyIx damagedEnemies
            forM_ damagedEnemies $ \PhaserDamageResult {..} ->
              sayDialog Combat (generateDamageDialog _phaserDamageReportHitPointDamage _phaserDamageReportEnemy)
          PhaserManual -> pure ()

data PhaserDamageResult
  = PhaserDamageResult
  { _phaserDamageReportHitPointDamage :: Int
  , _phaserDamageReportShieldValueDamage :: Int
  , _phaserDamageReportEnemyIdx :: Int
  , _phaserDamageReportEnemy :: Enemy
  }

resultToEnemyIx :: PhaserDamageResult -> (Int, Enemy)
resultToEnemyIx PhaserDamageResult {..} =
  (_phaserDamageReportEnemyIdx, _phaserDamageReportEnemy)

calculateEnemyPhaserDamage
  :: Int
  -> (Int, Enemy)
  -> State GameState PhaserDamageResult
calculateEnemyPhaserDamage amt (idx, enemy) = do
  factor <- randomPhaserFactor
  let dmgAmount = ceiling (fromIntegral amt * factor) - enemy^.Enemy.shieldValue
      shieldDmg = 2 -- TODO (james): figure out a good formula
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

enemyInRange :: (Int, Int) -> (Int, Int) -> Enemy -> Bool
enemyInRange (topLeftX, topLeftY) (offSetX, offSetY) Enemy {..} =
  enemyPositionX >= topLeftX
  && (topLeftX + offSetX) >= enemyPositionX
  && enemyPositionY >= topLeftY
  && (topLeftY + offSetY) >= enemyPositionY

enemiesInRange :: (Int, Int) -> (Int, Int) -> Array Int Enemy -> [(Int, Enemy)]
enemiesInRange rangeBoxCorner rangeBoxOffset
  = filter (enemyInRange rangeBoxCorner rangeBoxOffset . snd)
  . Array.assocs

generateDamageDialog :: Int -> Enemy -> Text
generateDamageDialog amt Enemy {..}
  = "Damaged enemy ship ("
  <> (T.pack . show $ enemyPositionX)
  <> ", "
  <> (T.pack . show $ enemyPositionY)
  <> ") for "
  <> (T.pack . show $ amt)

randomPhaserFactor :: State GameState Float
randomPhaserFactor = do
  gen <- use gameStateRandomGen
  let (f, gen') = randomR (0.9, 1.1) gen
  gameStateRandomGen .= gen'
  pure f
