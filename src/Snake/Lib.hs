{-# LANGUAGE FlexibleInstances #-}

module Snake.Lib
  ( withVertexAttribArray
  , texVertices

  , drawQuads
  , drawTrigs
  , drawPrimArray2d

  , quad
  , quadTurn
  , tau
  , unturn
  , turnTo
  , ToQuad(..)

  , frac
  , seconds
  , sometimes
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (void, when)
import Data.StateVar (($=))
import Data.Vector.Storable (Vector)
import Linear (V2(..), V4(..), unangle)
import GHC.Float (float2Double, float2Int, int2Float)

import qualified SDL
import qualified Apecs.System.Random as Random
import qualified Data.Vector.Storable as Vector
import qualified Graphics.Rendering.OpenGL as GL
import qualified System.Random.MWC.Probability as MWC

import Snake.Components (SystemW)

withVertexAttribArray :: MonadIO m => GL.AttribLocation -> Vector Float -> m () -> m ()
withVertexAttribArray location vertices action = do
  GL.vertexAttribArray location $= GL.Enabled
  liftIO . Vector.unsafeWith vertices $ \ptr ->
    GL.vertexAttribPointer location $=
      ( GL.ToFloat
      , GL.VertexArrayDescriptor 2 GL.Float 0 ptr
      )
  action
  GL.vertexAttribArray location $= GL.Disabled

-- | Generic quad 1-to-1 vertex-to-texture mapping
texVertices :: Vector Float
texVertices = Vector.fromList
  [ 0, 1
  , 0, 0
  , 1, 0
  , 1, 1
  ]

quad :: Float -> Float -> Float -> Float -> Vector Float
quad width height tx ty = Vector.fromListN 8
  [ tx - width * 0.5, ty - height * 0.5
  , tx - width * 0.5, ty + height * 0.5
  , tx + width * 0.5, ty + height * 0.5
  , tx + width * 0.5, ty - height * 0.5
  ]

-- | XXX: Consider using shader instead
quadTurn :: Float -> Float -> Float -> Float -> Float -> Vector Float
quadTurn width height tx ty turns = Vector.fromListN 8
  [ turnedX nnx nny, turnedY nnx nny
  , turnedX npx npy, turnedY npx npy
  , turnedX ppx ppy, turnedY ppx ppy
  , turnedX pnx pny, turnedY pnx pny
  ]
  where
    theta = turns * 2 * pi

    cosT = cos theta
    sinT = sin theta

    turnedX x y = tx + x * cosT - y * sinT
    turnedY x y = ty + x * sinT + y * cosT

    nnx = negate width * 0.5
    nny = negate height * 0.5

    npx = negate width * 0.5
    npy = height * 0.5

    pnx = width * 0.5
    pny = negate height * 0.5

    ppx = width * 0.5
    ppy = height * 0.5
{-# INLINE quadTurn #-}

unturn :: V2 Float -> Float
unturn vec = unangle vec / tau
{-# INLINE unturn #-}

turnTo :: V2 Float -> V2 Float -> Float
turnTo a b = unturn (a - b)
{-# INLINE turnTo #-}

tau :: Float
tau = pi * 2

class ToQuad a where
  toQuad :: a -> Vector Float

instance ToQuad (V4 Float) where
  toQuad (V4 width height tx ty) = quad width height tx ty
  {-# INLINE toQuad #-}

instance ToQuad (V2 Float) where
  toQuad (V2 width height) = quad width height 0 0
  {-# INLINE toQuad #-}

instance ToQuad (Float, Float, Float, Float) where
  toQuad (width, height, tx, ty) = quad width height tx ty
  {-# INLINE toQuad #-}

instance ToQuad (Float, Float) where
  toQuad (width, height) = quad width height 0 0
  {-# INLINE toQuad #-}

instance ToQuad (V2 Float, V2 Float) where
  toQuad (V2 width height, V2 left top) = quad width height left top
  {-# INLINE toQuad #-}

instance ToQuad (Float, V2 Float) where
  toQuad (size, V2 left top) = quad size size left top
  {-# INLINE toQuad #-}

drawQuads :: MonadIO m => GL.AttribLocation -> Vector Float -> m ()
drawQuads = drawPrimArray2d GL.Quads

drawTrigs :: MonadIO m => GL.AttribLocation -> Vector Float -> m ()
drawTrigs = drawPrimArray2d GL.Triangles

drawPrimArray2d :: MonadIO m => GL.PrimitiveMode -> GL.AttribLocation -> Vector Float -> m ()
drawPrimArray2d primitive coord2d vertices =
  withVertexAttribArray coord2d vertices $
    liftIO $ GL.drawArrays primitive 0 numVertices
  where
    numVertices = fromIntegral $ Vector.length vertices `div` 2
{-# INLINE drawPrimArray2d #-}

frac :: Float -> Float
frac x = x - int2Float (float2Int x)
{-# INLINE frac #-}

seconds :: (Fractional a, MonadIO m) => m a
seconds = fmap ((/ 1000) . fromIntegral) SDL.ticks

sometimes :: Float -> SystemW a -> SystemW ()
sometimes chance action = do
  result <- Random.sample $ MWC.bernoulli (float2Double chance)
  when result $ void action
