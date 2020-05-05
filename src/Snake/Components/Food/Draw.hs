module Snake.Components.Food.Draw where

import Linear (V2(..))
import GHC.Float (int2Float)
import Apecs (global, cmap, cmapM_)
import qualified Apecs

import Snake.Components.Textures (toTextureKey)
import Snake.Components (Level(..), Food(..), Position(..), SystemW)
import Snake.Components.Level.System (levelSize)
import qualified Snake.Config as Config
import qualified Snake.Programs.Sprite as Sprite
import Snake.Math (toReal)

draw :: SystemW ()
draw = cmapM_ \(Food food, Position pos) -> do
  size <- levelSize
  let posReal = toReal size Config.blockSize (int2Float <$> pos)
      tex = toTextureKey "food" food
  Sprite.draw tex Config.blockSize posReal
