module LambdaTrek.UI where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

infoPanel :: Widget ()
infoPanel = center (str "Right")

commandPallet :: Widget ()
commandPallet = vLimit 3 (center (str "Command"))

simDisplay :: Widget ()
simDisplay = center (str "Left")

ui :: Widget ()
ui =
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel
    (str "LambdaTrek")
    (simDisplay
      <+> vBorder
      <+> hLimitPercent 30 (infoPanel <=> hBorder <=> commandPallet))
