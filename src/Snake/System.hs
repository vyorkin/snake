module Snake.System
  ( sometimes
  ) where

import Control.Monad (void, when)
import GHC.Float (float2Double)
import qualified Apecs.System.Random as Random
import qualified System.Random.MWC.Probability as MWC

import Snake.Components (SystemW)

sometimes :: Float -> SystemW a -> SystemW ()
sometimes chance action = do
  result <- Random.sample $ MWC.bernoulli (float2Double chance)
  when result $ void action
