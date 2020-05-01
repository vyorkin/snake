{-# LANGUAGE NamedFieldPuns #-}

module Snake.Components.Snake.System
  ( SnakeComponents
  , new
  , destroy
  , tick
  , setDir
  ) where

import Prelude hiding (head, tail)

import Control.Lens ((.~))
import Apecs (Entity, Not(..), cmap, cmapM, cmapM_, newEntity, ($=))
import qualified Apecs.System.Random as Random
import Linear (V2(..), (^*))

import Snake.Config (Level(..))
import Snake.Components.Snake.Types
import Snake.Components (SystemW)

type SnakeComponents = (Snake)

new :: Level -> SystemW Entity
new Level{..} = do
  _dir <- Random.boundedEnum @Dir
  let _head = V2 (_width `div` 2) (_height `div` 2)
      _tail = []
      _size = 64
  newEntity (Snake {..})

tick :: Float -> SystemW ()
tick _dt = cmap $ \snake@(Snake{..}) -> snake { _head = _head + (dirToV2 _dir) }

destroy :: Entity -> SystemW ()
destroy e = e $= Not @SnakeComponents

setDir :: Dir -> SystemW ()
setDir v = cmap $ dir .~ v
