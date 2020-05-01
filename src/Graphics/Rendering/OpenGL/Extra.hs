{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Rendering.OpenGL.Extra where

import GHC.Float (float2Double)
import Linear (V2(..), V3(..), V4(..))
import Unsafe.Coerce (unsafeCoerce)
import qualified Graphics.Rendering.OpenGL as GL

glFloat :: Float -> GL.GLfloat
glFloat x = unsafeCoerce x
{-# INLINE glFloat #-}

glDouble :: Float -> GL.GLdouble
glDouble x = unsafeCoerce (float2Double x)
{-# INLINE glDouble #-}

class ToGL a where
  type GLType a
  toGL :: a -> GLType a

-- Scalars

instance ToGL Bool where
  type GLType Bool = Float
  toGL b = if b then 1.0 else 0.0
  {-# INLINE toGL #-}

-- R2-like

instance ToGL (GL.Vector2 a) where
  type GLType (GL.Vector2 a) = GL.Vector2 a

  toGL = id
  {-# INLINE toGL #-}

instance ToGL (V2 a) where
  type GLType (V2 a) = GL.Vector2 a

  toGL (V2 x y) = GL.Vector2 x y
  {-# INLINE toGL #-}

instance ToGL (a, a) where
  type GLType (a, a) = GL.Vector2 a

  toGL (x, y) = GL.Vector2 x y
  {-# INLINE toGL #-}

-- R3-like

instance ToGL (GL.Vector3 a) where
  type GLType (GL.Vector3 a) = GL.Vector3 a

  toGL = id
  {-# INLINE toGL #-}

instance ToGL (V3 a) where
  type GLType (V3 a) = GL.Vector3 a

  toGL (V3 x y z) = GL.Vector3 x y z
  {-# INLINE toGL #-}

instance ToGL (a, a, a) where
  type GLType (a, a, a) = GL.Vector3 a

  toGL (x, y, z) = GL.Vector3 x y z
  {-# INLINE toGL #-}

-- R4-like

instance ToGL (GL.Vector4 a) where
  type GLType (GL.Vector4 a) = GL.Vector4 a

  toGL = id
  {-# INLINE toGL #-}

instance ToGL (V4 a) where
  type GLType (V4 a) = GL.Vector4 a

  toGL (V4 x y z w) = GL.Vector4 x y z w
  {-# INLINE toGL #-}

instance ToGL (a, a, a, a) where
  type GLType (a, a, a, a) = GL.Vector4 a

  toGL (x, y, z, w) = GL.Vector4 x y z w
  {-# INLINE toGL #-}
