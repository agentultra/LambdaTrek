{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek.UI where

import Brick
import Brick.Forms
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Data.List (foldl')
import qualified Data.Text as Text
import LambdaTrek.Simulation.Dialog
import qualified LambdaTrek.Simulation.Sector as Sector
import LambdaTrek.State
import Lens.Micro

data Name
  = CommandField
  | SectorDialog
  deriving (Eq, Ord, Show)

infoPanel :: Widget Name
infoPanel = center (str "Right")

commandPallet :: Form GameState e Name -> Widget Name
commandPallet f =
  let commandErrorWidget = maybe emptyWidget (withAttr (attrName "highlight-error") . txtWrap) $ formState f^.gameStateCommandError
  in vLimit 3 (center (commandErrorWidget <=> renderForm f))

dialog :: Dialog -> Widget Name
dialog (Dialog crewmate msg) =
  str "> " <+> (padRight (Pad 0) . withAttr (attrName (fromCrew crewmate)) . txtWrap . crewmateTxt $ crewmate) <+> txtWrap msg
  where
    fromCrew = \case
      Helm -> "highlight-helm"

    crewmateTxt = \case
      Helm -> "HELM: "

-- TODO: figure out why vLimit isn't actually working..
sectorDialog :: GameState -> Widget Name
sectorDialog gameState = withVScrollBars OnRight . viewport SectorDialog Vertical . combineDialogs $ gameState^.gameStateDialog
  where
    combineDialogs :: [Dialog] -> Widget Name
    combineDialogs = foldl' (<=>) emptyWidget . map dialog

sectorDisplay :: GameState -> Widget Name
sectorDisplay gameState =
  let sector = gameState^.gameStateSector
      ship = gameState^.gameStateShip
      sectorTiles = Sector.buildSectorTiles ship sector
  in (center . str . Text.unpack . Sector.render $ sectorTiles)
       <=> hBorder
       <=> vLimit 5 (sectorDialog gameState)

simDisplay :: Form GameState e Name -> Widget Name
simDisplay f = sectorDisplay $ formState f

ui :: Form GameState e Name -> Widget Name
ui f =
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel
    (str "LambdaTrek")
    (simDisplay f
      <+> vBorder
      <+> hLimitPercent 30 (infoPanel <=> hBorder <=> commandPallet f))

mkForm :: GameState -> Form GameState e Name
mkForm =
  let label s w
        = vLimit 1 $ hLimitPercent 80 $ str s <+> fill ' ' <+> w
  in newForm
     [ label "Command: " @@=
       editTextField gameStateCommandInput CommandField (Just 1)
     ]
