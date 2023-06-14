module LambdaTrek.Render where

import Brick.Forms
import Brick.Types
import LambdaTrek.State
import LambdaTrek.UI

lambdaRender :: Form GameState e Name -> [Widget Name]
lambdaRender f = [ui f]
