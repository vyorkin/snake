{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Snake.Components.Snake.Types where

import Apecs.TH (makeMapComponents)
import Control.Lens.TH (makeLenses)

data Snake = Snake
  { _size :: !Int
  } deriving (Show)

makeLenses ''Snake

newtype Cell = Cell
  { unCell :: Direction
  } deriving (Show)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Eq, Ord, Show, Enum, Bounded)

makeMapComponents
  [ ''Snake
  , ''Cell
  ]
