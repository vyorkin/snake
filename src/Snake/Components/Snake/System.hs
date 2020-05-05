{-# LANGUAGE NamedFieldPuns #-}

module Snake.Components.Snake.System
  ( SnakeComponents
  , spawn
  , new
  , destroy
  , tick
  , setDir
  ) where

import Debug.Trace (traceM)
import Control.Monad (when, void)
import Control.Lens ((^.), (.~), (%~), (+~))
import Apecs (Entity, Not(..), global, cmap, cmapM, cmapM_, newEntity, ($=), ($~))
import qualified Apecs
import qualified Apecs.System.Random as Random
import GHC.Float (float2Int)
import Linear (V2(..), (^*))

import qualified Snake.Config as Config
import Snake.Components
import Snake.Components.Level.System (levelSize, levelCenter)
import qualified Snake.Components.Food.System as Food
import Snake.Math (toReal)

type SnakeComponents = (Player, Snake, Velocity)

newSnake :: SystemW Snake
newSnake = do
  dir <- Random.boundedEnum
  hd  <- levelCenter >>= genBlock
  pure $ Snake
    { _snakeDir = dir
    , _snakeBody = pure hd
    , _snakeEating = False
    , _snakeTimer = 0.0
    }

tick :: Float -> SystemW ()
tick dt = eat >> move dt

move :: Float -> SystemW ()
move dt = cmapM_ \(Snake{..}, Velocity vel, snake) -> do
  let oldHead = head _snakeBody
      oldPos = _snakeBlockPos oldHead
      newPos = oldPos + dirToV2 _snakeDir ^* float2Int _snakeTimer
      newHead = oldHead { _snakeBlockPos = newPos }
  snake $~ snakeBody %~ \body ->
    let nextBody = if _snakeEating then id else init
     in newHead:(nextBody body)
  snake $~ snakeEating .~ False
  snake $~ snakeTimer %~ \t ->
    if t > 1.0 then 0.0 else t + dt * vel

eat :: SystemW ()
eat =
  cmapM_ \(Snake{..}, snake) ->
  cmapM_ $ \(Food{..}, Position foodPos, food) -> do
    let snakePos = _snakeBlockPos $ head _snakeBody
    when (snakePos == foodPos) do
      snake $~ snakeEating .~ True
      Food.destroy food

spawn :: SystemW ()
spawn = void new

new :: SystemW Entity
new = do
  snake <- newSnake
  vel <- newVelocity
  newEntity (Player, snake, vel)

genBlock :: V2 Int -> SystemW SnakeBlock
genBlock pos = do
  color <- Random.boundedEnum
  pure $ SnakeBlock
    { _snakeBlockColor = color
    , _snakeBlockPos = pos
    }

newVelocity :: SystemW Velocity
newVelocity = do
  Level{..} <- Apecs.get global
  pure $ Velocity _levelSnakeSpeed

destroy :: Entity -> SystemW ()
destroy e = e $= Not @SnakeComponents

setDir :: Dir -> SystemW ()
setDir v = cmap $ snakeDir .~ v
