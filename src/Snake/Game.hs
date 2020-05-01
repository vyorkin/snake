module Snake.Game
  ( run
  ) where

import Debug.Trace (traceM)
import Data.StateVar (($=))

import qualified Apecs
import qualified Apecs.System.Random as Random
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL

import Snake.Config (Config(..))
import Snake.Components as Components
import qualified Snake.Window as Window
import qualified Snake.Scene as Scene
import Snake.MainLoop (mainLoop)

run :: Config -> IO ()
run Config{..} = do
  SDL.initializeAll
  SDL.HintRenderScaleQuality $= SDL.ScaleNearest -- ScaleLinear

  let windowCfg = glWindow SDL.FullscreenDesktop -- Fullscree, Windowed
  window <- SDL.createWindow _name windowCfg
  SDL.showWindow window

  _glContext <- SDL.glCreateContext window

  -- Enable VSync
  SDL.swapInterval $= SDL.SynchronizedUpdates

  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  world <- Components.initWorld
  Apecs.runWith world $ do
    windowSize <- SDL.glGetDrawableSize window
    traceM $ "Initial window: " <> show windowSize
    Window.setSize windowSize
    Scene.init _level
    mainLoop window

  SDL.destroyWindow window
  SDL.quit

glWindow :: SDL.WindowMode -> SDL.WindowConfig
glWindow mode = SDL.defaultWindow
  { SDL.windowGraphicsContext = SDL.OpenGLContext glConfig
  , SDL.windowResizable = True
  , SDL.windowMode = mode
  }
  where
    glConfig  = SDL.defaultOpenGL { SDL.glProfile = glProfile }
    -- The compatibility profile allows you to
    -- use deprecated functions such as immediate mode
    glProfile = SDL.Compatibility SDL.Normal 3 0
    -- GL 4.4 is the maximum version supported by
    -- my Intel® HD Graphics 620
