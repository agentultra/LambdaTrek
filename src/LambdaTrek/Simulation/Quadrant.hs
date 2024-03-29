{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaTrek.Simulation.Quadrant where

import Data.Array hiding ((!))
import qualified Data.Array as Array
import Data.List.Split
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map, (!))
import qualified Data.Map as M
import LambdaTrek.Simulation.Enemy
import LambdaTrek.Simulation.Sector.Internal
import LambdaTrek.Simulation.Station
import Lens.Micro.TH

data Quadrant
  = Quadrant
  { _quadrantStars      :: Map (Int, Int) [(Int, Int)]
  , _quadrantEnemyShips :: Map (Int, Int) [Enemy]
  , _quadrantStations   :: Map (Int, Int) [Station]
  , _quadrantScanState  :: Map (Int, Int) Bool
  }
  deriving (Eq, Show)

quadrantCoords :: [(Int, Int)]
quadrantCoords = [(x, y) | x <- [0..3], y <- [0..3]]

addStar :: (Int, Int) -> (Int, Int) -> Quadrant -> Maybe Quadrant
addStar sectorCoord starCoord quadrant
  | availableSpace quadrant sectorCoord = Just
    quadrant { _quadrantStars = M.adjust ((:) starCoord) sectorCoord quadrant._quadrantStars }
  | otherwise = Nothing

addEnemy :: (Int, Int) -> Enemy -> Quadrant -> Maybe Quadrant
addEnemy sectorCoord enemy quadrant
  | availableSpace quadrant sectorCoord = Just
    quadrant { _quadrantEnemyShips = M.adjust ((:) enemy) sectorCoord quadrant._quadrantEnemyShips }
  | otherwise = Nothing

addStation :: (Int, Int) -> Station -> Quadrant -> Maybe Quadrant
addStation sectorCoord station quadrant
  | availableSpace quadrant sectorCoord = Just
    quadrant { _quadrantStations = M.adjust ((:) station) sectorCoord quadrant._quadrantStations }
  | otherwise = Nothing

-- | Check if there's available space to place an object /other than/
-- the player ship.  There is always one space reserved for the player.
availableSpace :: Quadrant -> (Int, Int) -> Bool
availableSpace quadrant sectorCoord =
  let sector = getSector quadrant sectorCoord
      numEnemyShips = length $ listEnemies sector
      numStations = length $ listStars sector
      numStars = length $ listStations sector
  in 15 * 15 - (numEnemyShips + numStations + numStars) > 1

initQuadrant :: Quadrant
initQuadrant =
  let sectorStarMap
        = M.fromList
        [ (coord, [])
        | coord <- quadrantCoords
        ]
      sectorEnemyShipMap
        = M.fromList
        [ (coord, [])
        | coord <- quadrantCoords
        ]
      sectorStationMap
        = M.fromList
        [ (coord, [])
        | coord <- quadrantCoords
        ]
  in Quadrant
     { _quadrantStars = sectorStarMap
     , _quadrantEnemyShips = sectorEnemyShipMap
     , _quadrantStations = sectorStationMap
     , _quadrantScanState = mempty
     }

getSector :: Quadrant -> (Int, Int) -> Sector
getSector Quadrant {..} sectorCoord =
  let stars' = _quadrantStars ! sectorCoord
      enemies = _quadrantEnemyShips ! sectorCoord
      stations' = _quadrantStations ! sectorCoord
      arrEnemies = toArray enemies
      arrStation = toArray stations'
  in Sector stars' arrEnemies arrStation

toArray :: [a] -> Array Int a
toArray xs = listArray (0, length xs - 1) xs

updateSector :: Quadrant -> (Int, Int) -> Sector -> Quadrant
updateSector quadrant sectorCoord Sector {..} =
  quadrant { _quadrantStars = M.adjust (const sectorStars) sectorCoord (_quadrantStars quadrant)
           , _quadrantEnemyShips = M.adjust (const $ elems sectorEnemyShips) sectorCoord (_quadrantEnemyShips quadrant)
           , _quadrantStations = M.adjust (const $ elems sectorStations) sectorCoord (_quadrantStations quadrant)
           }

scanQuadrant :: (Int, Int) -> Quadrant -> Quadrant
scanQuadrant coord quadrant =
  let scanState = _quadrantScanState quadrant
  in quadrant { _quadrantScanState = M.alter (const $ Just True) coord scanState }

data QuadrantTileData
  = QuadrantTileData
    { quadrantTileHasPlayerShip :: Bool
    , quadrantTileHasEnemyShips :: Bool
    , quadrantTileHasStarbase   :: Bool
    }
  deriving (Eq, Show)

data QuadrantTile = Scanned QuadrantTileData | Unscanned
  deriving (Eq, Show)

toQuadrantTile :: Bool -> Bool -> Sector -> QuadrantTile
toQuadrantTile isShipPresent hasBeenScanned Sector {..}
  | hasBeenScanned = Scanned QuadrantTileData
    { quadrantTileHasPlayerShip = isShipPresent
    , quadrantTileHasEnemyShips = any enemyAlive . Array.elems $ sectorEnemyShips
    , quadrantTileHasStarbase   = not . null . Array.elems $ sectorStations
    }
  | otherwise = Unscanned
  where
    enemyAlive Enemy {..} = enemyHitPoints > 0

renderTile :: Int -> [QuadrantTile] -> Text
renderTile 0 ts = T.intercalate " " $ replicate (length ts) "+-----+"
renderTile 1 ts = T.intercalate " " $ replicate (length ts) "|     |"
renderTile 2 ts = T.intercalate " " $ map renderQuadrantData ts
renderTile 3 ts = T.intercalate " " $ replicate (length ts) "|     |"
renderTile 4 ts = T.intercalate " " $ replicate (length ts) "+-----+"
renderTile _ _ = ""

renderQuadrantData :: QuadrantTile -> Text
renderQuadrantData = \case
  Scanned QuadrantTileData {..} ->
    "| " <> renderHasShips <> renderHasStations <> renderHasPlayerShip <> " |"
    where
      renderHasShips = if quadrantTileHasEnemyShips then "<" else " "
      renderHasStations = if quadrantTileHasStarbase then "$" else " "
      renderHasPlayerShip = if quadrantTileHasPlayerShip then "S" else " "
  Unscanned -> "|  ?  |"

newtype QuadrantTiles
  = QuadrantTiles { getQuadrantTiles :: [QuadrantTile] }
  deriving (Eq, Show)

getTile :: Int -> Int -> QuadrantTiles -> Maybe QuadrantTile
getTile x y tiles
  | x >= 0 && x <= 15 && y >= 0 && y <= 15 =
    let quadrantData = getQuadrantTiles tiles
    in Just $ quadrantData !! ((y * 16) + x)
  | otherwise = Nothing

buildTiles :: (Int, Int) -> Quadrant -> QuadrantTiles
buildTiles currentSector quadrant =
  QuadrantTiles . map getQuadrantTileData $ quadrantCoords
  where
    getQuadrantTileData :: (Int, Int) -> QuadrantTile
    getQuadrantTileData quadrantCoordinate =
      let sectorScanned = M.findWithDefault False quadrantCoordinate (_quadrantScanState quadrant)
      in toQuadrantTile (currentSector == quadrantCoordinate) sectorScanned . getSector quadrant $ quadrantCoordinate

render :: QuadrantTiles -> Text
render
  = T.intercalate "\n"
  . map renderRow
  . chunksOf 4
  . getQuadrantTiles
  where
    renderRow :: [QuadrantTile] -> Text
    renderRow tiles
      = T.intercalate "\n"
      . map (uncurry renderTile)
      $ [(textRowIx, tiles) | textRowIx <- [0..4]]

makeLenses ''Quadrant
