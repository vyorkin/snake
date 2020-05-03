module Snake.Components.Food.Draw where

import Control.Lens ((&), (.~))
import Linear (V2(..))
import qualified Apecs
import GHC.Float (int2Float)

import Snake.Components.Textures (toTextureKey)
import Snake.Components.Food.Types
import Snake.Config (Level(..))
import qualified Snake.Config as Config
import Snake.Components (SystemW)
import qualified Snake.Programs.Sprite as Sprite

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

toReal :: Level -> V2 Int -> V2 Float
toReal Level{_levelWidth = lw, _levelHeight = lh} pos =
  let
    (fw, fh) = (int2Float lw, int2Float lh)
    V2 cw ch = Config.cellSize
    (sx, sy) = (cw * 0.25, ch * 0.25)
    (xMin, yMin) = (-fw * sx, -fh * sy)
    real mv v = mv + cw * 0.5 * v
    V2 px py = int2Float <$> pos
  in
    V2 (real xMin px) (real yMin py)
