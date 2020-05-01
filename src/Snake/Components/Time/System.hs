module Snake.Components.Time.System
  ( unlessPaused
  , togglePause
  ) where

import Apecs (Not(..), get, global, ($=))

import Snake.Components (SystemW)
import Snake.Components.Time.Types (Pause(..))

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
