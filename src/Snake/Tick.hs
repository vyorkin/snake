module Snake.Tick
  ( tick
  ) where

import Snake.Components (SystemW)
import qualified Snake.Components.Snake.System as Snake
import qualified Snake.Components.Time.System as Time

tick :: Float -> SystemW ()
tick _dt = pure ()
-- tick _dt = Time.unlessPaused do
--   Snake.tick dt
  -- Motion.tick dt
  -- Collision.tick dt
