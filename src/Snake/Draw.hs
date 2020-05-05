{-# LANGUAGE NamedFieldPuns #-}

module Snake.Draw
  ( drawBackdrop
  , drawScene
  , drawUI
  ) where

import Data.Foldable (for_, traverse_)
import Linear (V2(..), V4(..), (^*))
import System.FilePath ((</>))
import GHC.Float (float2Int, int2Float)
import qualified Graphics.Rendering.OpenGL as GL
import Apecs (global)
import qualified Apecs

import Snake.Components (SystemW, Level(..), Window(..))
import qualified Snake.Window as Window
import qualified Snake.Programs.Textured as Textured
import qualified Snake.Lib as Lib
import qualified Snake.Config as Config (blockSize)
import qualified Snake.Components.Textures as Textures
import qualified Snake.Components.Snake.Draw as Snake
import qualified Snake.Components.Food.Draw as Food
import Snake.Programs.Solid as Solid

drawBackdrop :: SystemW ()
drawBackdrop = do
  window <- Apecs.get global
  let side = Window.sizeMax window
      tex = Textures.Key $ "bg" </> "1"
  Textured.drawWith tex \coord2d ->
    Lib.drawQuads coord2d $ Lib.toQuad $ V4 1 1 0 0 ^* side

-- drawField :: SystemW ()
-- drawField = do
--   let
--     (fw, fh) = (int2Float lw, int2Float lh)
--     V2 cw ch = Config.blockSize
--     (sx, sy) = (cw * 0.25, ch * 0.25)
--     (xMin, xMax) = (-fw * sx, fw * sx)
--     (yMin, yMax) = (-fh * sy, fh * sy)
--     real mv v = mv + cw * 0.5 * v
--     v4 = V4 cw ch
--     v4x ym x = v4 (real xMin x) ym
--     v4y xm y = v4 xm (real yMin y)
--     xs = int2Float <$> [0..lw]
--     ys = int2Float <$> [0..lh]
--     ct = map (v4x yMin) xs
--     cb = map (v4x yMax) xs
--     cl = map (v4y xMin) ys
--     cr = map (v4y xMax) ys
--   for_ [ct, cb, cl, cr] $ traverse_ drawCell
--   where
--     tex = Textures.Key $ "field" </> "quad"
--     drawCell cell = Textured.drawWith tex \coord2d ->
--       Lib.drawQuads coord2d $ Lib.toQuad cell

drawScene :: SystemW ()
drawScene = do
  Snake.draw
  Food.draw

drawUI :: SystemW ()
drawUI = Snake.drawUI
