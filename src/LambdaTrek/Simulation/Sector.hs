{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Sector where

import Data.Array
import Data.List (foldl', find)
import Data.List.Split
import Data.Text (Text)
import qualified Data.Text as Text
import LambdaTrek.Simulation.Enemy
import LambdaTrek.Simulation.Enemy.AI
import LambdaTrek.Simulation.Ship (Ship)
import qualified LambdaTrek.Simulation.Ship as Ship
import LambdaTrek.Simulation.Station (Station (..))
import qualified LambdaTrek.Simulation.Station as Station
import LambdaTrek.Simulation.Tile (Tile)
import qualified LambdaTrek.Simulation.Tile as Tile
import Lens.Micro
import Lens.Micro.TH

-- | Sectors are 15x15 tiled regions of space
data Sector
  = Sector
  { sectorStars :: [(Int, Int)]
  , sectorEnemyShips :: Array Int Enemy
  , sectorStations :: Array Int Station
  }
  deriving (Eq, Ord, Show)

makeFields ''Sector

emptySector :: Sector
emptySector = Sector [] (listArray (0,0) [Enemy 8 3 20 10 Patrolling 10]) (listArray (0,0) [Station 9 1 100])

newtype SectorTiles = SectorTiles { getSectorTiles :: Array Int Int }
  deriving (Eq, Ord, Show)

emptySectorTiles :: SectorTiles
emptySectorTiles
  = SectorTiles
  $ listArray (0, (15 * 15) - 1) (repeat $ fromEnum Tile.EmptySpace)

getTile :: Int -> Int -> SectorTiles -> Maybe Tile
getTile x y sectorTiles
  | x >= 0 && x <= 14 && y >= 0 && y <= 14 =
    let sectorData = getSectorTiles sectorTiles
    in Just . toEnum $ sectorData ! ((y * 15) + x)
  | otherwise = Nothing

setTile :: Int -> Int -> Tile -> SectorTiles -> Maybe SectorTiles
setTile x y tile sectorTiles
  | x >= 0 && x <= 14 && y >= 0 && y <= 14 =
    Just . unsafeSetTile x y tile $ sectorTiles
  | otherwise = Nothing

unsafeSetTile :: Int -> Int -> Tile -> SectorTiles -> SectorTiles
unsafeSetTile x y tile sectorTiles =
  let sectorData = getSectorTiles sectorTiles
  in SectorTiles $ sectorData // [((y * 15) + x, fromEnum tile)]

buildSectorTiles :: Ship -> Sector -> SectorTiles
buildSectorTiles ship sector =
  let starterTiles = unsafeSetTile (ship^.Ship.positionX) (ship^.Ship.positionY) Tile.PlayerShip emptySectorTiles
      starTiles = foldl' addStar starterTiles $ sector^.stars
      enemyTiles = foldl' addEnemy starTiles $ sector^.enemyShips
  in foldl' addStation enemyTiles $ sector^.stations
  where
    addStar :: SectorTiles -> (Int, Int) -> SectorTiles
    addStar tiles (x, y) =
      unsafeSetTile x y Tile.Star tiles

    addEnemy :: SectorTiles -> Enemy -> SectorTiles
    addEnemy tiles enemy
      | isDestroyed enemy = unsafeSetTile
        (enemy^.positionX) (enemy^.positionY) Tile.DestroyedEnemyShip tiles
      | otherwise =
        unsafeSetTile (enemy^.positionX) (enemy^.positionY) Tile.EnemyShip tiles

    addStation :: SectorTiles -> Station -> SectorTiles
    addStation tiles station = unsafeSetTile (station^.Station.positionX) (station^.Station.positionY) Tile.Station tiles

render :: SectorTiles -> Text
render sector =
  let sectorData = getSectorTiles sector
  in Text.intercalate "\n"
     . map Text.concat
     . chunksOf 15
     . map (Tile.render . toEnum)
     . elems
     $ sectorData

aliveEnemies :: Sector -> [(Int, Enemy)]
aliveEnemies = filter (isAlive . snd) . assocs . sectorEnemyShips

enemyAtCoord :: (Int, Int) -> Sector -> Maybe (Int, Enemy)
enemyAtCoord coord
  = find (isAtCoordinate coord . snd)
  . assocs
  . sectorEnemyShips
  where
    isAtCoordinate :: (Int, Int) -> Enemy -> Bool
    isAtCoordinate (coordX, coordY) Enemy {..} =
      coordX == enemyPositionX && coordY == enemyPositionY
