{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LambdaTrek.UI where

import Brick
import Brick.Forms
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Table
import Data.List (foldl')
import qualified Data.Text as Text
import qualified Graphics.Vty.Attributes as Vty
import qualified Graphics.Vty.Image as Vty
import LambdaTrek.Simulation.Dialog
import qualified LambdaTrek.Simulation.Quadrant as Quadrant
import qualified LambdaTrek.Simulation.Sector as Sector
import qualified LambdaTrek.Simulation.Ship as Ship
import LambdaTrek.State
import Lens.Micro

data Name
  = CommandField
  | SectorDialog
  deriving (Eq, Ord, Show)

infoPanel :: Form GameState e Name -> Widget Name
infoPanel f =
  let gameState = formState f
  in center . renderTable
     $ table
     [ [str "Energy:", str $ show (gameState^.gameStateShip.Ship.energy)]
     , [str "Hull:", str $ show (gameState^.gameStateShip.Ship.hull)]
     , [str "Shields:", str $ Text.unpack (Ship.shieldStateText $ gameState^.gameStateShip.Ship.shieldState)]
     , [str "Shield Strength:", str $ displayShieldStrength (gameState^.gameStateShip.Ship.shieldStrength)]
     , [str "Torpedos:", str $ show (gameState^.gameStateShip.Ship.torpedos)]
     , [str "Remaining Turns:", str $ show (gameState^.gameStateRemainingTurns)]
     ]
  where
    displayShieldStrength :: Double -> String
    displayShieldStrength d =
      let amt = floor $ d * 100
      in show @Integer amt ++ "%"

commandPallet :: Form GameState e Name -> Widget Name
commandPallet f =
  let commandErrorWidget = maybe emptyWidget (withAttr (attrName "highlight-error") . txtWrap) $ formState f^.gameStateCommandError
  in vLimit 3 (center (commandErrorWidget <=> renderForm f))

dialog :: Dialog -> Widget Name
dialog (Dialog crewmate msg) =
  let dialogStr = Vty.string (fromCrewmate crewmate) (crewmateTxt crewmate)
        Vty.<|> Vty.string Vty.defAttr (Text.unpack msg)
  in raw dialogStr
  where
    fromCrewmate :: Crewmate -> Vty.Attr
    fromCrewmate = \case
      Helm -> Vty.defAttr `Vty.withForeColor` Vty.yellow
      Combat -> Vty.defAttr `Vty.withForeColor` Vty.red
      Engineering -> Vty.defAttr `Vty.withForeColor` Vty.blue
    crewmateTxt = \case
      Helm -> "HELM: "
      Combat -> "COMBAT: "
      Engineering -> "ENGINEERING: "

-- TODO: figure out why vLimit isn't actually working..
sectorDialog :: GameState -> Widget Name
sectorDialog gameState = withVScrollBars OnRight . viewport SectorDialog Vertical . combineDialogs $ gameState^.gameStateDialog
  where
    combineDialogs :: [Dialog] -> Widget Name
    combineDialogs = foldl' (<=>) emptyWidget . map dialog

sectorDisplay :: GameState -> Widget Name
sectorDisplay gameState =
  let sector = Quadrant.getSector (gameState^.gameStateQuadrant) $ gameState^.gameStateSector
      ship = gameState^.gameStateShip
      sectorTiles = Sector.buildSectorTiles ship sector
  in (center . str . Text.unpack . Sector.render $ sectorTiles)
       <=> hBorder
       <=> vLimit 5 (sectorDialog gameState)

sectorScreen :: Form GameState e Name -> Widget Name
sectorScreen f = sectorDisplay $ formState f

gameOverScreen :: Form GameState e Name -> Widget Name
gameOverScreen f =
  let gameState = formState f
  in str ("Game Over: " ++ show (gameState^.(gameStateShip . Ship.energy)))

ui :: Form GameState e Name -> Widget Name
ui f =
  let gameState = formState f
      gameScreen = case gameState^.gameStateScreen of
        SectorScreen ->
          sectorScreen f
          <+> vBorder
          <+> hLimitPercent 30 (infoPanel f <=> hBorder <=> commandPallet f)
        GameOverScreen -> gameOverScreen f
  in joinBorders $
     withBorderStyle unicode $
     borderWithLabel
     (str "LambdaTrek")
     gameScreen


mkForm :: GameState -> Form GameState e Name
mkForm =
  let label s w
        = vLimit 1 $ hLimitPercent 80 $ str s <+> fill ' ' <+> w
  in newForm
     [ label "Command: " @@=
       editTextField gameStateCommandInput CommandField (Just 1)
     ]
