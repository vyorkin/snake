module Snake.Scene
  ( init
  ) where

import Prelude hiding (init)

import Linear (V2(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Apecs.System.Random as Random

import Snake.Components (SystemW)
import Snake.Config (Level(..))
import qualified Snake.Components.Snake.System as Snake
import qualified Snake.Components.Food.System as Food

init :: Level -> SystemW ()
init level@Level{..} = do
  _ <- Snake.new level
  sequence_ . replicate _levelFoodSpawn $
    Food.spawn level
