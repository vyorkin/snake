module Snake.Components.Snake.Draw
  ( draw
  , drawUI
  ) where

import System.FilePath ((</>))
import Control.Monad (forM_)
import Control.Lens ((&), (.~))
import Linear (V2(..), lerp, (^*))
import GHC.Float (int2Float)
import Apecs (cmapM_)
import qualified Apecs.System.Random as Random
import qualified Apecs

import qualified Snake.Config as Config
import Snake.Components
import Snake.Components.Textures (toTextureKey)
import Snake.Components.Level.System (levelSize)
import qualified Snake.Components.Textures as Textures
import qualified Snake.Programs.Sprite as Sprite
import Snake.Math (toReal)

draw :: SystemW ()
draw = cmapM_ \(Snake{..}) -> forM_ _snakeBody drawBlock

drawBlock :: SnakeBlock -> SystemW ()
drawBlock SnakeBlock{..} = do
  size <- levelSize
  let tex = toTextureKey "snake" _snakeBlockColor
      pos = toReal size Config.blockSize (int2Float <$> _snakeBlockPos)
  Sprite.draw tex Config.blockSize pos

drawUI :: SystemW ()
drawUI = pure ()
