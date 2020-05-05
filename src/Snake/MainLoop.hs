module Snake.MainLoop
  ( mainLoop
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.StateVar (($=))
import Linear (V4(..), (^*))
import Apecs (global)
import qualified Apecs
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL

import Graphics.Rendering.OpenGL.Extra (glDouble)
import Snake.Components (SystemW, Window(..), Camera(..))
import Snake.Tick (tick)
import qualified Snake.Events as Events
import qualified Snake.Draw as Draw
import qualified Snake.Window as Window

mainLoop :: SDL.Window -> SystemW ()
mainLoop sdlWindow = do
  (camera@Camera{..}, window) <- Apecs.get global

  let
    windowQuad = glDouble <$> Window.quad window
    resetProjection factor = liftIO do
      GL.loadIdentity
      let V4 xStart xEnd yStart yEnd = windowQuad ^* factor
      GL.ortho xStart xEnd yStart yEnd 0 (-100)

  quit <- Events.handle window camera
  unless quit do
    tick (1 / 60)

    setViewport window
    clearScreen

    resetProjection 1.0
    Draw.drawBackdrop
    resetProjection (glDouble _cameraScale)
    Draw.drawScene
    resetProjection 1.0
    Draw.drawUI

    SDL.glSwapWindow sdlWindow
    mainLoop sdlWindow
  where
    setViewport Window{..} = liftIO $ GL.viewport $=
      ( GL.Position 0 0
      , GL.Size (truncate _windowWidth) (truncate _windowHeight)
      )
    clearScreen = liftIO do
      GL.clearColor $= GL.Color4 0 0 0 1
      GL.clear [GL.ColorBuffer]
