{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Snake.Components.Snake.Types where

import Linear (V2(..))
import Apecs.TH (makeMapComponents)
import Control.Lens.TH (makeLenses)

data Snake = Snake
  { _dir  :: !Dir
  , _body :: ![V2 Int]
  } deriving (Show)

data Dir = U | D | L | R
  deriving (Eq, Ord, Show, Enum, Bounded)

dirToV2 :: Dir -> V2 Int
dirToV2 = \case
  U -> V2 0 (-1)
  D -> V2 0 1
  L -> V2 (-1) 0
  R -> V2 1 0

makeMapComponents
  [ ''Snake
  ]

makeLenses ''Snake
