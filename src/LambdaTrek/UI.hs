module LambdaTrek.UI where

import Brick
import Brick.Forms
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import LambdaTrek.State

data Name = CommandField
  deriving (Eq, Ord, Show)

infoPanel :: Widget Name
infoPanel = center (str "Right")

commandPallet :: Form GameState e Name -> Widget Name
commandPallet f = vLimit 3 (center (renderForm f))

simDisplay :: Form GameState e Name -> Widget Name
simDisplay f =
  let currentCommand = _gameStateCommand . formState $ f
  in center (str . show $ currentCommand)

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
