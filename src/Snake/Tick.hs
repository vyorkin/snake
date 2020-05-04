module Snake.Tick
  ( tick
  ) where

import Snake.Config (Level)
import Snake.Components (SystemW)
import qualified Snake.Components.Snake.System as Snake
import qualified Snake.Components.Food.System as Food
import qualified Snake.Components.Time.System as Time

tick :: Level -> Float -> SystemW ()
tick level dt = Time.unlessPaused do
  Snake.tick dt
  Food.tick level dt
