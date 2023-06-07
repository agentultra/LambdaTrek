module LambdaTrek where

import Brick (simpleMain)
import LambdaTrek.UI

run :: IO ()
run = simpleMain ui
