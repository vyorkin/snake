{-# LANGUAGE NamedFieldPuns #-}

module Snake.Components.Snake.System
  ( SnakeComponents
  , spawn
  , new
  , destroy
  , tick
  , setDir
  ) where

import Control.Monad (when, void)
import Control.Lens ((^.), (.~), (%~), _head)
import Apecs (Entity, Not(..), global, cmap, cmapM_, newEntity, ($=), ($~))
import qualified Apecs
import qualified Apecs.System.Random as Random
import GHC.Float (float2Int)
import Linear (V2(..), (^*))

import Snake.Components
import Snake.Components.Level.System (levelCenter)
import qualified Snake.Components.Food.System as Food

type SnakeComponents = (Player, Snake)

newSnake :: SystemW Snake
newSnake = do
  let _snakeEating = False
  _snakeDir <- Random.boundedEnum
  _snakeBody <- pure <$> levelCenter
  _snakeColor <- Random.boundedEnum
  pure Snake{..}

tick :: SystemW ()
tick = collide >> eat >> move

collide :: SystemW ()
collide = cmapM_ \(snake@Snake{..}, snakeEty) ->
  when (overlapsItself snake) $ destroy snakeEty

eat :: SystemW ()
eat =
  cmapM_ \(snake, snakeEty) ->
  cmapM_ \(Food{..}, Position foodPos, foodEty) -> do
    when (nextPos snake == foodPos) do
      Food.destroy foodEty
      snakeEty $~ snakeEating .~ True

move :: SystemW ()
move = cmapM_ \(s, ety) ->
  if s^.snakeEating
  then do
    ety $~ snakeEating .~ False
    ety $~ snakeBody %~ ((:) (nextPos s))
  else
    ety $~ snakeBody %~ ((:) (nextPos s) . init)

overlapsItself :: Snake -> Bool
overlapsItself s = nextPos s `elem` tail (s^.snakeBody)

nextPos :: Snake -> V2 Int
nextPos s = head (s^.snakeBody) + dirToV2 (s^.snakeDir)

spawn :: SystemW ()
spawn = void new

new :: SystemW Entity
new = newSnake >>= \s -> newEntity (Player, s)

destroy :: Entity -> SystemW ()
destroy e = e $= Not @SnakeComponents

setDir :: Dir -> SystemW ()
setDir v = cmap $ snakeDir .~ v
