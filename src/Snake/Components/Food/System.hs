module Snake.Components.Food.System
  ( FoodComponents
  , spawn
  , tick
  , destroy
  ) where

import Control.Monad (when, void)
import Apecs (Entity, Not(..), cmap, cmapM, cmapM_, newEntity, ($=), ($~))
import qualified Apecs.System.Random as Random
import Control.Lens ((+~))
import GHC.Float (int2Float, float2Int)
import Linear (V2(..), (^*))

import Snake.Config (Level(..))
import Snake.Components.Food.Types
import Snake.Components (SystemW)
import Snake.System (sometimes)

type FoodComponents = (Food)

spawn :: Level -> SystemW Entity
spawn Level{..} = do
  x <- Random.range (1, _levelWidth - 1)
  y <- Random.range (1, _levelHeight - 1)
  _foodType <- Random.boundedEnum
  let _foodPos = V2 x y
      _foodTimer = 0.0
  newEntity Food{..}

tick :: Level -> Float -> SystemW ()
tick level@Level{..} dt = cmapM_ \(Food{..}, food) -> do
  food $~ foodTimer +~ dt
  when (_foodTimer > int2Float _levelFoodTTL) $
    spawn level *> destroy food

destroy :: Entity -> SystemW ()
destroy e = e $= Not @FoodComponents
