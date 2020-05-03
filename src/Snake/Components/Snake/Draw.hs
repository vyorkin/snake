module Snake.Components.Snake.Draw
  ( draw
  , drawUI
  ) where

import System.FilePath ((</>))
import Control.Monad (forM_)
import Control.Lens ((&), (.~))
import Linear (V2(..), lerp, (^*))
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Float (int2Float)
import qualified Apecs.System.Random as Random
import qualified Apecs

import Snake.Config (Level(..))
import qualified Snake.Config as Config
import Snake.Components (SystemW)
import Snake.Components.Snake.Types
import Snake.Components.Textures (toTextureKey)
import qualified Snake.Components.Textures as Textures
import qualified Snake.Programs.Sprite as Sprite
import Snake.Math (toReal)

draw :: Level -> SystemW ()
draw level = Apecs.cmapM_ \Snake{..} -> do
  drawCell level _snakeHead
  forM_ _snakeTail (drawCell level)

drawCell :: Level -> SnakeCell -> SystemW ()
drawCell level SnakeCell{..} = do
  let tex = toTextureKey "snake" _snakeCellColor
      pos = toReal level _snakeCellPos
  Sprite.draw mempty tex Config.cellSize pos

drawUI :: SystemW ()
drawUI = pure ()
