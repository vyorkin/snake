module Snake.Components.Level.System
  ( levelSize
  , levelCenter
  , genPosition
  ) where

import Linear (V2(..))
import Apecs (global)
import qualified Apecs
import qualified Apecs.System.Random as Random

import Snake.Components (SystemW, Level(..), Position(..))

levelSize :: SystemW (V2 Int)
levelSize = do
  Level{..} <- Apecs.get global
  pure $ V2 _levelWidth _levelHeight

levelCenter :: SystemW (V2 Int)
levelCenter = do
  V2 x y <- levelSize
  pure $ V2 (x `div` 2) (y `div` 2)

genPosition :: SystemW Position
genPosition = do
  V2 lw lh <- levelSize
  x <- Random.range (1, lw - 1)
  y <- Random.range (1, lh - 1)
  pure $ Position (V2 x y)
