module Snake.Scene
  ( init
  ) where

import Prelude hiding (init)

import Linear (V2(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Apecs.System.Random as Random

import Snake.Components (SystemW)
import Snake.Config (Level(..))
import qualified Snake.Components.Snake.System as Snake

init :: Level -> SystemW ()
init level@Level{..} = do
  snake <- Snake.new level
  -- sequence_ . replicate _food $
  --   Food.spawnRandom _levelWidth _levelHeight
  pure ()
