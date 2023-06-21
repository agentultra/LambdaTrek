{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek.Simulation.Sector where

import Data.Array
import Data.List.Split
import Data.Text (Text)
import qualified Data.Text as Text
import LambdaTrek.Simulation.Ship (Ship)
import qualified LambdaTrek.Simulation.Ship as Ship
import LambdaTrek.Simulation.Tile (Tile (..))
import qualified LambdaTrek.Simulation.Tile as Tile

data Enemy = Enemy deriving (Eq, Ord, Show)

data Sector
  = Sector
  { _sectorStars :: [(Int, Int)]
  , _sectorEnemyShips :: [Enemy]
  }
  deriving (Eq, Ord, Show)

emptySector :: Sector
emptySector = Sector [] []

newtype SectorTiles = SectorTiles { getSectorTiles :: Array Int Int }
  deriving (Eq, Ord, Show)

emptySectorTiles :: SectorTiles
emptySectorTiles
  = SectorTiles
  $ listArray (0, (15 * 15) - 1) (repeat $ fromEnum EmptySpace)

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
buildSectorTiles ship _ =
  unsafeSetTile (Ship._shipX ship) (Ship._shipY ship) PlayerShip emptySectorTiles

render :: SectorTiles -> Text
render sector =
  let sectorData = getSectorTiles sector
  in Text.intercalate "\n"
     . map Text.concat
     . chunksOf 15
     . map (Tile.render . toEnum)
     . elems
     $ sectorData
