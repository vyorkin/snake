{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Snake.Components
  ( World
  , SystemW
  , initWorld

  , Window(..)
  , windowWidth
  , windowHeight
  , windowScale

  , Camera(..)
  , cameraOffset
  , cameraScale
  ) where

import Apecs
import Apecs.TH (makeMapComponents)
import Control.Lens.TH (makeLenses)
import Linear (V2)

import Snake.Components.Time.Types as Time
import Snake.Components.Snake.Types (Snake)
import qualified Snake.Components.Snake.Types as Snake

data Window = Window
  { _windowWidth  :: Float
  , _windowHeight :: Float
  , _windowScale  :: Float
  }
  deriving (Eq, Show)

instance Component Window where
  type Storage Window = Unique Window

data Camera = Camera
  { _cameraScale :: Float
  , _cameraOffset :: V2 Float
  } deriving (Eq, Show)

instance Semigroup Camera where
  a <> b = Camera
    { _cameraScale  = _cameraScale a * _cameraScale b
    , _cameraOffset = _cameraOffset a + _cameraOffset b
    }

instance Monoid Camera where
  mempty = Camera
    { _cameraScale  = 1.0
    , _cameraOffset = 0
    }

instance Component Camera where
  type Storage Camera = Global Camera

makeWorld "World"
  [ ''Window
  , ''Camera
  , ''Snake
  , ''Time.Pause
  ]

type SystemW a = SystemT World IO a

makeLenses ''Camera
makeLenses ''Window
