module Snake.Components.Time.System
  ( tick
  , unlessPaused
  , togglePause
  ) where

import Apecs (Not(..), get, global, ($=))
import qualified Apecs

import Snake.Components (SystemW, Time(..), Pause(..))

tick :: Float -> SystemW ()
tick dt = Apecs.modify global $ \(Time t) -> Time (t + dt)

unlessPaused :: SystemW () -> SystemW ()
unlessPaused action =
  get global >>= \case
    Just Pause ->
      pure ()
    Nothing ->
      action

togglePause :: SystemW ()
togglePause =
  get global >>= \case
    Nothing ->
      global $= Pause
    Just Pause ->
      global $= Not @Pause
