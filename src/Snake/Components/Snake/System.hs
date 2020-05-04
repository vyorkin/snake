{-# LANGUAGE NamedFieldPuns #-}

module Snake.Components.Snake.System
  ( SnakeComponents
  , new
  , destroy
  , tick
  , setDir
  ) where

import Prelude hiding (head, tail)

import Control.Monad (when)
import Control.Lens ((.~), (%~), (+~))
import Apecs (Entity, Not(..), cmap, cmapM, cmapM_, newEntity, ($=), ($~))
import qualified Apecs.System.Random as Random
import GHC.Float (float2Int)
import Linear (V2(..), (^*))

import Snake.Config (Level(..))
import Snake.Components.Snake.Types
import Snake.Components (SystemW)

type SnakeComponents = (Snake)

new :: Level -> SystemW Entity
new Level{..} = do
  _snakeDir <- Random.boundedEnum
  _snakeCellColor <- Random.boundedEnum
  let _snakeCellPos = V2 (_levelWidth `div` 2) (_levelHeight `div` 2)
      _snakeCellTimer = 0.0
      _snakeHead = SnakeCell{..}
      _snakeTail = []
      _snakeSpeed = _levelSnakeSpeed
      _snakeTimer = 0.0
  newEntity Snake{..}

tick :: Float -> SystemW ()
tick dt = cmapM_ \(Snake{..}, snake) -> do
  let delta = dirToV2 _snakeDir ^* float2Int _snakeTimer
  snake $~ snakeHead.snakeCellPos +~ delta
  snake $~ snakeTimer %~ \t ->
    if t > 1.0
    then 0.0
    else t + dt * _snakeSpeed

destroy :: Entity -> SystemW ()
destroy e = e $= Not @SnakeComponents

setDir :: Dir -> SystemW ()
setDir v = cmap $ snakeDir .~ v
