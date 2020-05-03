module Snake.Math
  ( toReal
  ) where

import Linear (V2(..))
import GHC.Float (int2Float)

import Snake.Config (Level(..))
import qualified Snake.Config as Config

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
