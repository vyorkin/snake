module Snake.Draw
  ( drawBackdrop
  , drawScene
  , drawUI
  ) where

import Linear (V4(..), (^*))
import System.FilePath ((</>))

import qualified Apecs

import Snake.Components (SystemW)

drawBackdrop :: SystemW ()
drawBackdrop = pure ()

drawScene :: SystemW ()
drawScene = pure ()

drawUI :: SystemW ()
drawUI = pure ()
