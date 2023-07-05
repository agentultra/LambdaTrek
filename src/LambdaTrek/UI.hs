module LambdaTrek.UI where

import Brick
import Brick.Forms
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Data.Text as Text
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

-- TODO: figure out why vLimit isn't actually working..
sectorDialog :: Widget Name
sectorDialog = withVScrollBars OnRight $ viewport SectorDialog Vertical $ str ""

sectorDisplay :: GameState -> Widget Name
sectorDisplay gameState =
  let sector = gameState^.gameStateSector
      ship = gameState^.gameStateShip
      sectorTiles = Sector.buildSectorTiles ship sector
  in center ((str . Text.unpack . Sector.render $ sectorTiles)
             <=> hBorder
             <=> vLimit 5 sectorDialog)

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
