module Snake.Components.Food.Draw where

import Control.Lens ((&), (.~))
import qualified Apecs

import Snake.Components.Textures (toTextureKey)
import Snake.Components.Food.Types
import Snake.Config (Level(..))
import qualified Snake.Config as Config
import Snake.Components (SystemW)
import qualified Snake.Programs.Sprite as Sprite
import Snake.Math (toReal)

draw :: Level -> SystemW ()
draw level = Apecs.cmapM_ \Food{..} -> do
  let
    pos = toReal level _foodPos
    tex = toTextureKey "food" _foodType
    eff =
      if _foodTimer < 2.0
      then mempty & Sprite.effectsOutline .~ Just 1.0
      else mempty
  Sprite.draw eff tex Config.cellSize pos
