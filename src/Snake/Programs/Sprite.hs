{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Snake.Programs.Sprite where

import Control.Applicative ((<|>))
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (MonadIO(..))
import Data.StateVar (($=))
import GHC.Stack (HasCallStack)
import Linear (V2(..), V3(..), V4(..))

import qualified Apecs
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.OpenGL.Extra (toGL)
import qualified Snake.Components.Programs as Programs
import qualified Snake.Components.Textures as Textures
import qualified Snake.Lib as Lib

data Effects = Effects
  { _effectsOutline :: Maybe (V4 Float)
  , _effectsTurn    :: Float
  , _effectsGamma   :: V3 Float
  , _effectsOpacity :: Float
  } deriving (Show)

instance Semigroup Effects where
  a <> b = Effects
    { _effectsOutline = _effectsOutline b <|> _effectsOutline a
    , _effectsTurn    = Lib.frac $ _effectsTurn a + _effectsTurn b
    , _effectsGamma   = _effectsGamma a * _effectsGamma b
    , _effectsOpacity = _effectsOpacity a * _effectsOpacity b
    }

instance Monoid Effects where
  mempty = Effects
    { _effectsOutline = Nothing
    , _effectsTurn    = 0.0
    , _effectsGamma   = 1.0
    , _effectsOpacity = 1.0
    }

draw
  :: ( Apecs.Has w m Programs.Programs
     , Apecs.Has w m Textures.Textures
     , HasCallStack
     , MonadIO m
     )
  => Effects
  -> Textures.Key
  -> V2 Float
  -> V2 Float
  -> Apecs.SystemT w m ()
draw Effects{..} key size pos = do
  Textures.Texture{textureObject} <- Textures.get key

  Programs.withCompiled program $ \setUniform withAttribute -> do
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just textureObject
    setUniform "texture" $ GL.TextureUnit 0
    setUniform "u_invGamma" $ toGL (recip _effectsGamma)
    setUniform "u_opacity" _effectsOpacity

    withAttribute "texcoord" $ \texcoord ->
      Lib.withVertexAttribArray texcoord Lib.texVertices $
        withAttribute "coord2d" $ \coord2d ->
          Lib.drawQuads coord2d $
            Lib.quadTurn width height x y _effectsTurn

  case _effectsOutline of
    Nothing ->
      pure ()
    Just color ->
      outline key size color pos _effectsTurn

  where
    program = Programs.Key "texture"

    V2 width height = size
    V2 x y = pos

outline
  :: ( Apecs.Has w m Programs.Programs
     , Apecs.Has w m Textures.Textures
     , MonadIO m
     )
  => Textures.Key
  -> V2 Float
  -> V4 Float
  -> V2 Float
  -> Float
  -> Apecs.SystemT w m ()
outline key quadSize color pos turn = do
  Textures.Texture{..} <- Textures.get key

  Programs.withCompiled program $ \setUniform withAttribute -> do
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just textureObject
    setUniform "texture" $ GL.TextureUnit 0
    setUniform "u_color" $ toGL color

    setUniform "u_stepSize" $
      GL.Vector2 @Float
        (1 / fromIntegral textureWidth)
        (1 / fromIntegral textureHeight)

    withAttribute "texcoord" $ \texcoord ->
      Lib.withVertexAttribArray texcoord Lib.texVertices $
        withAttribute "coord2d" $ \coord2d ->
          Lib.drawQuads coord2d $
            Lib.quadTurn width height x y turn

  where
    program = Programs.Key "outline"

    V2 width height = quadSize
    V2 x y = pos

makeLenses ''Effects
