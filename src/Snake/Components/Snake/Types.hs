{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Snake.Components.Snake.Types where

import Linear (V2(..))
import Apecs.TH (makeMapComponents)
import Control.Lens.TH (makeLenses)

data SnakeCellColor
  = Blue
  | Green
  | Pink
  | Yellow
  deriving (Eq, Show, Enum, Bounded)

data SnakeCell = SnakeCell
  { _snakeCellColor :: !SnakeCellColor
  , _snakeCellPos :: !(V2 Int)
  , _snakeCellTimer :: !Float
  } deriving (Show)

data Snake = Snake
  { _snakeDir :: !Dir
  , _snakeHead :: !SnakeCell
  , _snakeTail :: ![SnakeCell]
  , _snakeSpeed :: !Float
  } deriving (Show)

data Dir = U | D | L | R
  deriving (Eq, Ord, Show, Enum, Bounded)

dirToV2 :: Dir -> V2 Int
dirToV2 = \case
  U -> V2 0 1
  D -> V2 0 (-1)
  L -> V2 (-1) 0
  R -> V2 1 0

makeMapComponents [''Snake]
makeLenses ''SnakeCell
makeLenses ''Snake
