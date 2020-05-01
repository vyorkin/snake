module Snake.Components.Field.Draw where

import Control.Lens ((&), (.~))
import Linear (V3(..), V4(..), lerp, (^*))
import qualified Apecs

import Snake.Components (SystemW)
import qualified Snake.Components.Textures as Textures
import qualified Snake.Programs.Sprite as Sprite

draw :: SystemW ()
draw = pure ()
