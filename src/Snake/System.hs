module Snake.System
  ( every
  , sometimes
  ) where

import Control.Monad (void, when)
import GHC.Float (float2Double)
import Apecs (global)
import qualified Apecs.System.Random as Random
import qualified Apecs
import qualified System.Random.MWC.Probability as MWC

import Snake.Components (SystemW, Time(..))

sometimes :: Float -> SystemW a -> SystemW ()
sometimes chance action = do
  result <- Random.sample $ MWC.bernoulli (float2Double chance)
  when result $ void action

every :: Float -> Float -> Float -> SystemW a -> SystemW ()
every dt period phase sys = do
  Time t <- Apecs.get global
  let t' = t + phase
      t1 = floor (t' / period) :: Int
      t2 = floor ((t' + dt) / period)
  when (t1 /= t2) $ void sys
