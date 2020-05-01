module Snake.Draw
  ( drawBackdrop
  , drawScene
  , drawUI
  ) where

import Linear (V4(..), (^*))
import System.FilePath ((</>))

import qualified Apecs

import Snake.Components (SystemW)
import qualified Snake.Window as Window
import qualified Snake.Programs.Textured as Textured
import qualified Snake.Lib as Lib
import qualified Snake.Components.Textures as Textures

drawBackdrop :: SystemW ()
drawBackdrop = do
  window <- Apecs.get Apecs.global
  let side = Window.sizeMax window
  Textured.drawWith (Textures.Key $ "bg" </> "1") $ \coord2d -> do
    Lib.drawQuads coord2d $ Lib.toQuad $ V4 1 1 0 0 ^* side

drawScene :: SystemW ()
drawScene = pure ()

drawUI :: SystemW ()
drawUI = pure ()
