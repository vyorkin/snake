{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Snake.Programs.Solid where

import Control.Monad.IO.Class (MonadIO)
import GHC.Stack (HasCallStack)

import qualified Apecs
import qualified Graphics.Rendering.OpenGL as GL

import qualified Snake.Components.Programs as Programs
import Graphics.Rendering.OpenGL.Extra (ToGL, GLType, toGL)

drawWith
  :: ( Apecs.Has w m Programs.Programs
     , HasCallStack
     , MonadIO m
     , ToGL color
     , GLType color ~ GL.Vector4 Float
     )
  => color
  -> (GL.AttribLocation -> Apecs.SystemT w m ())
  -> Apecs.SystemT w m ()
drawWith color4 action =
  Programs.withCompiled (Programs.Key "solid") $ \setUniform withAttribute -> do
    setUniform "mycolor" $ toGL color4
    withAttribute "coord2d" action
