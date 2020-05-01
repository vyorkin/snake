module Snake.Components.Snake.Draw
  ( draw
  , drawUI
  ) where

import System.FilePath ((</>))
import Control.Lens ((&), (.~))
import Linear (V2(..), lerp, (^*))
import qualified Apecs

import Snake.Components (SystemW)
import Snake.Components.Snake.Types (Snake(..))
import qualified Snake.Components.Textures as Textures
import qualified Snake.Programs.Sprite as Sprite
import GHC.Float (int2Float)

draw :: SystemW ()
draw =
  Apecs.cmapM_ $ \(Snake{..}) -> do
    let texture = Textures.Key $ "snake" </> "blue"
    Sprite.draw mempty texture (V2 _size _size) (int2Float <$> _head)

drawUI :: SystemW ()
drawUI = pure ()
