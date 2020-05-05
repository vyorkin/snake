module Snake.Math
  ( toReal
  ) where

import Linear (V2(..))
import GHC.Float (int2Float)

-- | Convert block position to a real position on the screen.
toReal
  :: V2 Int   -- ^ Level size
  -> V2 Float -- ^ Block size
  -> V2 Float -- ^ Block position
  -> V2 Float
toReal (V2 lw lh) (V2 bw bh) pos =
  let
    (fw, fh) = (int2Float lw, int2Float lh)
    (sx, sy) = (bw * 0.25, bh * 0.25)
    (xMin, yMin) = (-fw * sx, -fh * sy)
    real mv v = mv + bw * 0.5 * v
    V2 px py = pos
  in
    V2 (real xMin px) (real yMin py)
