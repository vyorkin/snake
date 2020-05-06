module Snake.Components.Food.Draw where

import Linear (V2(..))
import GHC.Float (int2Float)
import Apecs (global, cmap, cmapM_)
import qualified Apecs

import Snake.Components.Textures (toTextureKey)
import Snake.Components
import Snake.System (every)
import Snake.Components.Level.System (levelSize)
import qualified Snake.Config as Config
import qualified Snake.Programs.Sprite as Sprite
import Snake.Components.Delayed.Types (Delayed(..))
import Snake.Math (toReal)

draw :: SystemW ()
draw = cmapM_ \(Food food, LifeSpan t, Position pos) -> do
  size <- levelSize
  let posReal = toReal size Config.blockSize (int2Float <$> pos)
      tex = toTextureKey "food" food
  Sprite.draw tex (Config.blockSize * 0.8) posReal (min 1.0 t)
