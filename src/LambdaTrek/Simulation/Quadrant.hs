{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  }
  deriving (Eq, Show)

initQuadrant :: Quadrant
initQuadrant =
  let sectorStarMap
        = M.fromList
        [ ((0, 0), [(10, 10)])
        ]
      sectorEnemyShipMap
        = M.fromList
        [ ((0, 0), listArray (0,0) [Enemy 8 3 20 10 Patrolling 10])
        ]
      sectorStationMap
        = M.fromList
        [ ((0, 0), listArray (0,0) [Station 9 1 100])
        ]
  in Quadrant sectorStarMap sectorEnemyShipMap sectorStationMap

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

newtype QuadrantTile = QuadrantTile Int
  deriving (Eq, Show)

renderTile :: QuadrantTile -> Text
renderTile = T.pack . show

newtype QuadrantTiles = QuadrantTiles { getQuadrantTiles :: Array Int Int }

getTile :: Int -> Int -> QuadrantTiles -> Maybe QuadrantTile
getTile x y tiles
  | x >= 0 && x <= 15 && y >= 0 && y <= 15 =
    let quadrantData = getQuadrantTiles tiles
    in Just . QuadrantTile $ quadrantData Array.! ((y * 16) + x)
  | otherwise = Nothing

buildTiles :: Quadrant -> QuadrantTiles
buildTiles _ = QuadrantTiles $ listArray (0, 15) [1..]

render :: QuadrantTiles -> Text
render
  = T.intercalate "\n"
  . map renderRow
  . chunksOf 4
  . map (QuadrantTile . snd)
  . assocs
  . getQuadrantTiles
  where
    renderRow :: [QuadrantTile] -> Text
    renderRow = T.intercalate " | " . map renderTile

makeLenses ''Quadrant
