{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaTrek.Simulation.Combat where

import Control.Monad.State
import Data.Array
import qualified Data.Array as Array
import Data.Bifunctor
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

handleFirePhasers :: Int -> PhaserMode -> State GameState ()
handleFirePhasers energyAmt firingMode = do
  enemies <- getEnemiesInPhaserRange
  unless (null enemies) $
    case firingMode of
      PhaserAutomatic -> do
        let damageAmount = energyAmt `div` length enemies
            damagedEnemies = map (second $ Enemy.damageEnemy damageAmount) enemies
        gameStateShip %= Ship.subtractEnergy (damageAmount * length damagedEnemies)
        gameStateSector . enemyShips %= \ships -> ships Array.// damagedEnemies
        forM_ damagedEnemies $ \(_, damagedEnemy) ->
          sayDialog Combat (generateDamageDialog damageAmount damagedEnemy)
      PhaserManual -> pure ()

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
