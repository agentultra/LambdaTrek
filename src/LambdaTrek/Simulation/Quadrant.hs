{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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
import LambdaTrek.Simulation.Enemy.AI
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Station
import Lens.Micro.TH

data Quadrant
  = Quadrant
  { _quadrantStars :: Map (Int, Int) [(Int, Int)]
  , _quadrantEnemyShips :: Map (Int, Int) (Array Int Enemy)
  , _quadrantStations :: Map (Int, Int) (Array Int Station)
  , _quadrantScanState :: Map (Int, Int) Bool
  }
  deriving (Eq, Show)

quadrantCoords :: [(Int, Int)]
quadrantCoords = [(x, y) | x <- [0..3], y <- [0..3]]

initQuadrant :: (Int, Int) -> Quadrant
initQuadrant startingCoord =
  let sectorStarMap
        = M.fromList
        [ (coord, [(10, 10)]) | coord <- quadrantCoords ]
      sectorEnemyShipMap
        = M.fromList
        [ (coord, if coord == (0, 0) then listArray (0,0) [Enemy 8 3 20 10 Patrolling 10] else listArray (0, -1) [])
        | coord <- quadrantCoords
        ]
      sectorStationMap
        = M.fromList
        [ (coord, listArray (0,0) [Station 9 1 100])
        | coord <- quadrantCoords
        ]
  in Quadrant
     { _quadrantStars = sectorStarMap
     , _quadrantEnemyShips = sectorEnemyShipMap
     , _quadrantStations = sectorStationMap
     , _quadrantScanState = M.singleton startingCoord True
     }

getSector :: Quadrant -> (Int, Int) -> Sector
getSector Quadrant {..} sectorCoord =
  let stars' = _quadrantStars ! sectorCoord
      enemies = _quadrantEnemyShips ! sectorCoord
      stations' = _quadrantStations ! sectorCoord
  in Sector stars' enemies stations'

updateSector :: Quadrant -> (Int, Int) -> Sector -> Quadrant
updateSector quadrant sectorCoord Sector {..} =
  quadrant { _quadrantStars = M.adjust (const sectorStars) sectorCoord (_quadrantStars quadrant)
           , _quadrantEnemyShips = M.adjust (const sectorEnemyShips) sectorCoord (_quadrantEnemyShips quadrant)
           , _quadrantStations = M.adjust (const sectorStations) sectorCoord (_quadrantStations quadrant)
           }

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
      renderHasShips = if quadrantTileHasEnemyShips then "<" else "?"
      renderHasStations = if quadrantTileHasStarbase then "$" else "?"
      renderHasPlayerShip = if quadrantTileHasPlayerShip then "S" else " "
  Unscanned -> "| ??  |"

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