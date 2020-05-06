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
import Control.Lens ((.~), (%~))
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
  dir <- Random.boundedEnum
  hd  <- levelCenter >>= genBlock
  pure $ Snake
    { _snakeDir = dir
    , _snakeBody = pure hd
    , _snakeEating = False
    }

tick :: SystemW ()
tick = eat >> move

eat :: SystemW ()
eat =
  cmapM_ \(snake@Snake{..}, snakeEty) ->
  cmapM_ $ \(Food{..}, Position foodPos, foodEty) -> do
    when (nextPos snake == foodPos) do
      Food.destroy foodEty
      snakeEty $~ snakeEating .~ True

move :: SystemW ()
move = cmapM_ \(snake@Snake{..}, snakeEty) -> do
  let oldHead = head _snakeBody
      newPos  = nextPos snake
      newHead = oldHead { _snakeBlockPos = newPos }
  if _snakeEating
  then do
    snakeEty $~ snakeEating .~ False
    snakeEty $~ snakeBody %~ ((:) newHead)
  else
    snakeEty $~ snakeBody %~ ((:) newHead . init)

nextPos :: Snake -> V2 Int
nextPos Snake{..} = _snakeBlockPos (head _snakeBody) + dirToV2 _snakeDir

spawn :: SystemW ()
spawn = void new

new :: SystemW Entity
new = do
  snake <- newSnake
  newEntity (Player, snake)

genBlock :: V2 Int -> SystemW SnakeBlock
genBlock pos = do
  color <- Random.boundedEnum
  pure $ SnakeBlock
    { _snakeBlockColor = color
    , _snakeBlockPos = pos
    }

destroy :: Entity -> SystemW ()
destroy e = e $= Not @SnakeComponents

setDir :: Dir -> SystemW ()
setDir v = cmap $ snakeDir .~ v
