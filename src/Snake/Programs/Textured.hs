{-# LANGUAGE NamedFieldPuns #-}

module Snake.Programs.Textured where

import Data.StateVar (($=))
import qualified Graphics.Rendering.OpenGL as GL

import Snake.Components (SystemW)

import qualified Snake.Components.Programs as Programs
import qualified Snake.Components.Textures as Textures
import qualified Snake.Lib as Lib

drawWith
  :: Textures.Key
  -> (GL.AttribLocation -> SystemW ())
  -> SystemW ()
drawWith key action =
  Programs.withCompiled (Programs.Key "texture") $ \setUniform withAttribute -> do
    Textures.Texture{textureObject} <- Textures.get key

    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just textureObject
    setUniform "image" $ GL.TextureUnit 0
    setUniform "u_opacity" (1.0 :: Float)

    withAttribute "texcoord" $ \texcoord ->
      Lib.withVertexAttribArray texcoord Lib.texVertices $
        withAttribute "coord2d" action
