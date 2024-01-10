{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaTrek.Simulation.Sector
  ( -- * Types
    Sector
    -- * Constructors
  , sector
  , -- * Lenses
    enemyShips
  , stars
  , stations
  , -- * Functions
    aliveEnemies
  , buildSectorTiles
  , emptySectorTiles
  , enemyAtCoord
  , findEmpty
  , getTile
  , listEnemies
  , listStars
  , listStations
  , render
  , setTile
  )
where

import Data.Array
import Data.List (foldl', find, nub, nubBy)
import Data.List.Split
import Data.Text (Text)
import qualified Data.Text as Text
import LambdaTrek.Simulation.Enemy
import LambdaTrek.Simulation.Enemy.AI
import LambdaTrek.Simulation.Position
import LambdaTrek.Simulation.Sector.Internal
import LambdaTrek.Simulation.Ship (Ship (..))
import LambdaTrek.Simulation.Station (Station (..))
import qualified LambdaTrek.Simulation.Station as Station
import LambdaTrek.Simulation.Tile (Tile)
import qualified LambdaTrek.Simulation.Tile as Tile
import Lens.Micro

-- | Construct a valid 'Sector'.
--
-- A valid sector has at least one empty space at all times.  So that
-- we have somewhere to put the player ship when they warp in.
--
-- The maximum number of @stars@, @enemies@, and @stations@ considered
-- is 224 and they must be unique in position.
sector :: [(Int, Int)] -> [Enemy] -> [Station] -> Either String Sector
sector stars' enemies stations' =
  let inputStars = take (15 * 15 - 1) stars'
      validStars = nub inputStars
      inputEnemies = take (15 * 15 - 1) enemies
      validEnemies = nubBy (\ex ey -> getPosition ex == getPosition ey) inputEnemies
      inputStations = stations'
      validStations = nubBy (\sx sy -> getPosition sx == getPosition sy) inputStations
      --s = Sector validStars (toEnemyArray enemies) (toStationArray stations')
  in if length inputStars > length validStars
     then Left "Duplicate stars" else
       if length inputEnemies > length validEnemies
       then Left "Enemy at duplicate position"
       else
         if length inputStations > length validStations
         then Left "Station at duplication position"
         else
           let s = Sector validStars (toEnemyArray validEnemies) (toStationArray validStations)
           in if hasAtLeastOneEmptySpace s
              then Right s
              else Left "Invalid sector" -- TODO: make this more informative
  where
    toEnemyArray :: [Enemy] -> Array Int Enemy
    toEnemyArray es = listArray (0, length es - 1) es

    toStationArray :: [Station] -> Array Int Station
    toStationArray ss = listArray (0, length ss - 1) ss

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

findEmpty :: SectorTiles -> (Int, Int)
findEmpty = ixToCoord . fst . head . filter ((== Tile.EmptySpace) . toEnum . snd) . assocs . getSectorTiles
  where
    ixToCoord :: Int -> (Int, Int)
    ixToCoord x =
      let w = 15
      in (x `div` w, x `rem` w)

buildSectorTiles :: Sector -> SectorTiles
buildSectorTiles sector' =
  let starTiles = foldl' addStar emptySectorTiles $ sector'^.stars
      enemyTiles = foldl' addEnemy starTiles $ sector'^.enemyShips
  in foldl' addStation enemyTiles $ sector'^.stations
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

render :: Ship -> SectorTiles -> Text
render ship sectorTiles =
  let (shipX, shipY) = getPosition ship
      sectorTileArray = getSectorTiles . unsafeSetTile shipX shipY Tile.PlayerShip $ sectorTiles
  in Text.intercalate "\n"
     . map Text.concat
     . chunksOf 15
     . map (Tile.render . toEnum)
     . elems
     $ sectorTileArray

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
