module Snake.Scene
  ( init
  ) where

import Prelude hiding (init)

import Control.Monad (void)
import Apecs (newEntity)

import Snake.Components (Level(..), SystemW)
import qualified Snake.Components.Snake.System as Snake
import qualified Snake.Components.Food.System as Food

init :: Level -> SystemW ()
init level@Level{..} = do
  void $ newEntity level
  Snake.spawn
  Food.spawn
