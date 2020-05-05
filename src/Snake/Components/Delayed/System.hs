{-# LANGUAGE TypeFamilies #-}

module Snake.Components.Delayed.System
  ( tick
  , temp
  , once
  , once_
  , interval
  , cancel
  , cancelAll
  ) where

import Debug.Trace (traceM)
import Apecs (cmap, cmapM_, newEntity, ($=), ($~))
import qualified Apecs
import Apecs.Core (Entity(..))
import Control.Lens ((&), (.~), (-~))
import Control.Monad (void)
import Data.Proxy (Proxy(..))
import Data.Typeable (cast)

import Snake.Components (SystemW)
import Snake.Components.Delayed.Types

tick :: Float -> SystemW ()
tick dt = cmapM_ \(Delayed{..}, e) -> do
  if _delayedTime > dt then
    e $~ delayedTime -~ dt
  else
    case (cast _delayedAction, cast _delayedCleanup) of
      (Just action, Just cleanup) -> do
        Apecs.destroy e $ Proxy @Delayed
        if _delayedCancel then
          cleanup
        else do
          () <- action
          case _delayedFinish of
            Cleanup ->
              cleanup
            NoCleanup ->
              pure ()
      _ ->
        error "assert: action from another World"

temp :: Float -> SystemW Entity -> (Entity -> SystemW ()) -> SystemW Entity
temp timer action destroy = do
  ety <- action
  once timer (pure ()) (destroy ety)

once :: Float -> SystemW () -> SystemW () -> SystemW Entity
once timer action cleanup =
  newEntity Delayed
    { _delayedTime    = timer
    , _delayedCancel  = False
    , _delayedFinish  = Cleanup
    , _delayedAction  = action
    , _delayedCleanup = cleanup
    }

once_ :: Float -> SystemW a -> SystemW ()
once_ timer action =
  void $ once timer (void action) (pure ())

cancelAll :: SystemW ()
cancelAll = cmap $ delayedCancel .~ True

cancel :: Entity -> SystemW ()
cancel e =
  Apecs.get e >>= \case
    Nothing ->
      pure ()
    Just Delayed{} ->
      e $~ delayedCancel .~ True

-- | Self-restarting delayed action with state.
interval
  :: Float
  -> (Maybe a -> SystemW (Maybe a))
  -> (Maybe a -> SystemW ())
  -> SystemW Entity
interval timer action cleanup = do
  -- XXX: Convoluted way of re-using entities
  e <- newEntity initial
  e $= delayed (action' e Nothing) (cleanup Nothing)
  pure e
  where
    initial = delayed (pure ()) (pure ())

    delayed a c = Delayed
      { _delayedTime    = timer
      , _delayedCancel  = False
      , _delayedFinish  = NoCleanup
      , _delayedAction  = a
      , _delayedCleanup = c
      }

    action' e st =
      action st >>= \case
        Nothing ->
          cleanup st
        next ->
          e $= delayed (action' e next) (cleanup next)
