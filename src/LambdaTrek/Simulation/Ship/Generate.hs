module LambdaTrek.Simulation.Ship.Generate where

import LambdaTrek.Simulation.Quadrant
import LambdaTrek.Simulation.Sector
import LambdaTrek.Simulation.Ship
import System.Random

generateStartingShip :: Quadrant -> StdGen -> (Ship, (Int, Int), StdGen)
generateStartingShip quadrant stdGen =
  let (quadrantX, stdGen')  = uniformR (0, 3) stdGen
      (quadrantY, stdGen'') = uniformR (0, 3) stdGen'
      sector' = getSector quadrant (quadrantX, quadrantY)
  in case pick stdGen'' . getEmpty $ buildSectorTiles sector' of
    Nothing ->
      ( Ship quadrantX quadrantY 100 6 30 ShieldsDown 0.75 5 WarpFactorOne
      , (quadrantX, quadrantY)
      , stdGen''
      )
    Just ((sectorCoordX, sectorCoordY), stdGen''') ->
      ( Ship sectorCoordX sectorCoordY 100 6 30 ShieldsDown 0.75 5 WarpFactorOne
      , (quadrantX, quadrantY)
      , stdGen'''
      )

pick :: StdGen -> [a] -> Maybe (a, StdGen)
pick _ [] = Nothing
pick stdGen xs =
  let (ix, stdGen') = uniformR (0, length xs - 1) stdGen
  in Just (xs !! ix, stdGen')
