module Snake.Components.Food.Draw where

import System.FilePath ((</>))
import Control.Lens ((&), (.~))
import Linear (V3(..), V4(..), lerp, (^*))
import qualified Apecs

import Snake.Components.Food.Types
import Snake.Components (SystemW)
import qualified Snake.Components.Textures as Textures
import qualified Snake.Programs.Sprite as Sprite

draw :: SystemW ()
draw = pure ()
