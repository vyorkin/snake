module Snake.Window
  ( setSize
  , sizeMax
  , sizeMin
  , quad

  , screenToWindow
  , windowToCamera
  , cameraToWindow

  , CameraTest
  , onCamera
  ) where

import Apecs (get, global, ($=))
import Linear (V2(..), V4(..), (^*))

import Snake.Components (Camera(..), Window(..), SystemW)

setSize :: Integral a => V2 a -> SystemW ()
setSize windowSize =
  global $= Window
    { _windowWidth  = windowWidth
    , _windowHeight = windowHeight
    , _windowScale  = 1.0
    }
  where
    V2 windowWidth windowHeight = fmap fromIntegral windowSize

sizeMax :: Window -> Float
sizeMax Window{..} = max _windowWidth _windowHeight

sizeMin :: Window -> Float
sizeMin Window{..} = min _windowWidth _windowHeight

quad :: Window -> V4 Float
quad Window{..} =
  V4
    (negate halfWidth)
    (halfWidth)
    (negate halfHeight)
    (halfHeight)
  where
    halfWidth  = _windowWidth * 0.5 * _windowScale
    halfHeight = _windowHeight * 0.5 * _windowScale

screenToWindow :: Integral a => Window -> V2 a -> V2 Float
screenToWindow Window{..} pos = windowPos
  where
    screenPos = fmap fromIntegral pos * V2 1 (-1)
    windowPos = screenPos + V2 (negate halfWidth) halfHeight ^* _windowScale

    halfWidth  = _windowWidth * 0.5
    halfHeight = _windowHeight * 0.5

windowToCamera :: Camera -> V2 Float -> V2 Float
windowToCamera Camera{..} pos = pos ^* _cameraScale

cameraToWindow :: Camera -> V2 Float -> V2 Float
cameraToWindow Camera{..} pos = pos ^* recip _cameraScale

type CameraTest = Float -> Float -> Bool

onCamera :: Float -> SystemW CameraTest
onCamera margin = do
  (window@Window{..}, camera) <- get global

  let
    w0 = screenToWindow @Int window $ V2
      (truncate $ negate margin)
      (truncate $ negate margin)

    wWH = screenToWindow @Int window $ V2
      (truncate $ _windowWidth + margin)
      (truncate $ _windowHeight + margin)

    V2 minX maxY = windowToCamera camera w0
    V2 maxX minY = windowToCamera camera wWH

  pure $ \x y ->
    x > minX && x < maxX &&
    y > minY && y < maxY
