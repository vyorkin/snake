{-# LANGUAGE FlexibleContexts #-}

module Snake.Events
  ( handle
  ) where

import Apecs (get, global, liftIO)
import Debug.Trace (traceM)
import Data.Foldable (for_)
import Linear (V2(..))
import System.Exit (exitSuccess)

import qualified Apecs
import qualified SDL

import Snake.Components (Window(..), Camera(..), SystemW)
import qualified Snake.Components.Time.System as Time
import qualified Snake.Window as Window

onKeyDown :: SDL.Keysym -> SystemW ()
onKeyDown SDL.Keysym{..} = do
  case keysymKeycode of
    SDL.KeycodeEscape -> do
      SDL.quit
      liftIO exitSuccess
    SDL.KeycodePause ->
      Time.togglePause
    SDL.KeycodeP ->
      Time.togglePause
    _ ->
      pure ()

onKeyUp :: SDL.Keysym -> SystemW ()
onKeyUp SDL.Keysym{..} =
  pure ()

-- | Extract and convert SDL event data.
handle :: Window -> Camera -> SystemW Bool
handle window camera = do
  events <- SDL.pollEvents
  for_ events $ \e -> case SDL.eventPayload e of
    SDL.KeyboardEvent eventData ->
      onKeyboard eventData
    SDL.WindowShownEvent eventData ->
      onWindowShown eventData
    SDL.WindowSizeChangedEvent eventData ->
      onWindowSizeChanged eventData
    _ ->
      pure ()
  pure $ SDL.QuitEvent `elem` map SDL.eventPayload events
  where
    onKeyboard SDL.KeyboardEventData{..} =
      case keyboardEventKeyMotion of
        SDL.Pressed ->
          onKeyDown keyboardEventKeysym
        SDL.Released ->
          onKeyUp keyboardEventKeysym

    onWindowShown SDL.WindowShownEventData{..} = do
      size <- SDL.glGetDrawableSize windowShownEventWindow
      traceM $ "Window shown: " <> show size
      Window.setSize size

    onWindowSizeChanged SDL.WindowSizeChangedEventData{..} = do
      traceM $ "Window resized: " <> show windowSizeChangedEventSize
      let V2 width height = fmap fromIntegral windowSizeChangedEventSize
      Apecs.set Apecs.global $ Window
        { _windowWidth  = width
        , _windowHeight = height
        , _windowScale  = 1.0
        }
