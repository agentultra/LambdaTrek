{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek.Simulation.Sector where

import Data.Array
import Data.List.Split
import Data.Text (Text)
import qualified Data.Text as Text
import LambdaTrek.Simulation.Tile (Tile (..))
import qualified LambdaTrek.Simulation.Tile as Tile

newtype Sector = Sector { getSector :: Array Int Int }
  deriving (Eq, Ord, Show)

emptySector :: Sector
emptySector
  = Sector
  $ listArray (0, (15 * 15) - 1) (repeat $ fromEnum EmptySpace)

get :: Int -> Int -> Sector -> Maybe Tile
get x y sector
  | x >= 0 && x <= 14 && y >= 0 && y <= 14 =
    let sectorData = getSector sector
    in Just . toEnum $ sectorData ! ((y * 15) + x)
  | otherwise = Nothing

render :: Sector -> Text
render sector =
  let sectorData = getSector sector
  in Text.intercalate "\n"
     . map Text.concat
     . chunksOf 15
     . map (Tile.render . toEnum)
     . elems
     $ sectorData
