{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Snake.Programs.Sprite where

import Control.Monad.IO.Class (MonadIO(..))
import Data.StateVar (($=))
import GHC.Stack (HasCallStack)
import Linear (V2(..))

import qualified Apecs
import qualified Graphics.Rendering.OpenGL as GL

import qualified Snake.Components.Programs as Programs
import qualified Snake.Components.Textures as Textures
import qualified Snake.Lib as Lib

draw
  :: ( Apecs.Has w m Programs.Programs
     , Apecs.Has w m Textures.Textures
     , HasCallStack
     , MonadIO m
     )
  => Textures.Key
  -> V2 Float
  -> V2 Float
  -> Apecs.SystemT w m ()
draw key size pos = do
  Textures.Texture{textureObject} <- Textures.get key

  Programs.withCompiled program $ \setUniform withAttribute -> do
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just textureObject
    setUniform "image" $ GL.TextureUnit 0

    withAttribute "texcoord" $ \texcoord ->
      Lib.withVertexAttribArray texcoord Lib.texVertices $
        withAttribute "coord2d" $ \coord2d ->
          Lib.drawQuads coord2d $
            Lib.quad width height x y
  where
    program = Programs.Key "texture"

    V2 width height = size
    V2 x y = pos
