{-# LANGUAGE TypeFamilies #-}

module Snake.Components.Time.Types
  ( Pause(..)
  ) where

import Apecs (Component(..), Unique)

data Pause = Pause
  deriving (Show)

instance Component Pause where
  type Storage Pause = Unique Pause
