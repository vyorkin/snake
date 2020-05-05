module Snake.Tick
  ( tick
  ) where

import Control.Monad (when, forM_)
import Data.Proxy (Proxy(..))
import Apecs (Not(..), global, cmap, cmapM, cmapM_, destroy)
import GHC.Float (int2Float)
import qualified Apecs
import Debug.Trace (traceM)

import Snake.Components
import Snake.System (every)
import qualified Snake.Components.Snake.System as Snake
import qualified Snake.Components.Food.System as Food
import qualified Snake.Components.Time.System as Time
import qualified Snake.Components.Delayed.System as Delayed

tick :: Float -> SystemW ()
tick dt = Time.unlessPaused do
  Time.tick dt
  Delayed.tick dt
  Snake.tick dt
  spawn dt

spawn :: Float -> SystemW ()
spawn dt = do
  Level{..} <- Apecs.get global
  forM_ [0.._levelFoodSpawn] \i ->
   every dt _levelFoodTTL (int2Float i) $ Food.spawn
  -- sometimes (dt / 12) $ Bomb.spawn
  -- sometimes (dt / 15) $ Hole.spawn
  -- sometimes (dt / 10) $ Wall.spawn
