module Snake.Components.Food.System
  ( FoodComponents
  , spawn
  , destroy
  ) where

import Control.Monad (void)
import qualified Apecs
import Apecs (Entity, Not(..), global, newEntity, ($=))
import qualified Apecs.System.Random as Random

import Snake.Components
import Snake.Components.Level.System (genPosition)
import Snake.Components.Delayed.System (temp)

type FoodComponents = (Food, Position)

spawn :: SystemW ()
spawn = do
  Level{..} <- Apecs.get global
  void $ temp _levelFoodTTL new destroy

new :: SystemW Entity
new = do
  Level{..} <- Apecs.get global
  food <- genFood
  position <- genPosition
  newEntity (food, position)

destroy :: Entity -> SystemW ()
destroy e = e $= Not @FoodComponents

genFood :: SystemW Food
genFood = Food <$> Random.boundedEnum
