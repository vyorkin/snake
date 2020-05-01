module Snake.MainLoop
  ( mainLoop
  ) where

import qualified Apecs
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL

import Snake.Components (SystemW)

mainLoop :: SDL.Window -> SystemW ()
mainLoop _wnd = pure ()
