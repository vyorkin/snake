module Snake.Components.Snake.Draw
  ( draw
  , drawUI
  ) where

import Control.Monad (forM_)
import Control.Lens ((^.))
import GHC.Float (int2Float)
import Linear (V2)
import Apecs (cmapM_)

import qualified Snake.Config as Config
import Snake.Components
import Snake.Components.Textures (toTextureKey)
import Snake.Components.Level.System (levelSize)
import qualified Snake.Programs.Sprite as Sprite
import Snake.Math (toReal)

draw :: SystemW ()
draw = cmapM_ \s -> forM_ (s^.snakeBody) (drawBlock $ s^.snakeColor)

drawBlock :: Color -> V2 Int -> SystemW ()
drawBlock color position = do
  size <- levelSize
  let tex = toTextureKey "snake" color
      pos = toReal size Config.blockSize (int2Float <$> position)
  Sprite.draw tex Config.blockSize pos 1.0

drawUI :: SystemW ()
drawUI = pure ()
