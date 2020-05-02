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

draw :: Level -> SystemW ()
draw Level{..} = Apecs.cmapM_ \Snake{..} -> do
  drawCell _snakeHead
  forM_ _snakeTail drawCell

drawCell :: SnakeCell -> SystemW ()
drawCell SnakeCell{..} = do
  let tex = toTextureKey "snake" _snakeCellColor
  Sprite.draw mempty tex Config.cellSize (int2Float <$> _snakeCellPos)

drawUI :: SystemW ()
drawUI = pure ()
