module LambdaTrek.UI.WrapText where

import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty.Attributes as Vty
import Graphics.Vty.Image ((<->))
import qualified Graphics.Vty.Image as Vty
import Text.Wrap

renderWrapText :: Int -> Text -> Vty.Image
renderWrapText lineWidth line =
  let ls = wrapTextToLines defaultWrapSettings lineWidth line
  in foldl' (<->) Vty.emptyImage . map (Vty.string Vty.defAttr . Text.unpack) $ ls
