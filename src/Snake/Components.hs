{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Snake.Components where

import Apecs
import Apecs.TH (makeMapComponents)
import Control.Lens.TH (makeLenses)
import Data.Monoid (Sum(..))
import Linear (V2(..))
import Apecs.Components.Random (RandomGen)

import Snake.Components.Programs (Programs)
import Snake.Components.Textures (Textures)
import Snake.Components.Delayed.Types (Delayed)

data FoodType
  = Cake
  | Orange
  | Lemon
  | Cherry
  | Pear
  deriving (Eq, Show, Enum, Bounded)

calories :: FoodType -> Int
calories = \case
  Cake -> 3
  Pear -> 2
  _    -> 1

data Color
  = Blue
  | Green
  | Pink
  | Yellow
  deriving (Eq, Show, Enum, Bounded)

data BombType = TimeBomb Int | Mine
  deriving (Eq, Show)

newtype Score = Score Int
  deriving stock (Show)
  deriving newtype (Num)
  deriving (Semigroup, Monoid) via Sum Int

instance Component Score where type Storage Score = Global Score

newtype Time = Time Float
  deriving (Show)
  deriving newtype (Num)
  deriving (Semigroup, Monoid) via Sum Float

instance Component Time where type Storage Time = Global Time

newtype Position = Position { unPosition :: V2 Int } deriving (Eq, Ord, Show)
newtype Density = Density { unDensity :: Int } deriving (Eq, Ord, Show)
newtype Expiring = Expiring { unExpiring :: Bool } deriving (Show)
newtype Hole = Hole { unHole :: V2 Int } deriving (Show)
newtype Food = Food { unFood :: FoodType } deriving (Show)
newtype Wall = Wall { unWall :: Color } deriving (Show)
newtype Bomb = Bomb { unBomb :: BombType } deriving (Show)

data SnakeBlock = SnakeBlock
  { _snakeBlockColor :: !Color
  , _snakeBlockPos :: !(V2 Int)
  } deriving (Show)

data Snake = Snake
  { _snakeDir :: !Dir
  , _snakeBody :: ![SnakeBlock]
  , _snakeEating :: !Bool
  } deriving (Show)

data Dir = U | D | L | R
  deriving (Eq, Ord, Show, Enum, Bounded)

dirToV2 :: Dir -> V2 Int
dirToV2 = \case
  U -> V2 0 1
  D -> V2 0 (-1)
  L -> V2 (-1) 0
  R -> V2 1 0

data Player = Player deriving (Show)
data Pause = Pause deriving (Show)

instance Component Player where type Storage Player = Unique Player
instance Component Pause where type Storage Pause = Unique Pause

data Level = Level
  { _levelWidth :: !Int
  , _levelHeight :: !Int
  , _levelSnakeSpeed :: !Float
  , _levelFoodSpawn :: !Int
  , _levelFoodTTL :: !Float
  , _levelBombTTL :: !Float
  , _levelHoleTTL :: !Float
  , _levelWallTTL :: !Float
  } deriving (Show)

instance Component Level where type Storage Level = Unique Level

data Window = Window
  { _windowWidth :: Float
  , _windowHeight :: Float
  , _windowScale :: Float
  } deriving (Eq, Show)

instance Component Window where type Storage Window = Unique Window

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

instance Component Camera where type Storage Camera = Global Camera

makeLenses ''Level
makeLenses ''Camera
makeLenses ''Window
makeLenses ''SnakeBlock
makeLenses ''Snake

makeMapComponents
  [ ''Position
  , ''Density
  , ''Expiring
  , ''Hole
  , ''Food
  , ''Wall
  , ''Bomb
  , ''Snake
  ]

makeWorld "World"
  [ ''Textures
  , ''Programs

  , ''Position
  , ''Density
  , ''Expiring
  , ''Hole
  , ''Food
  , ''Wall
  , ''Bomb
  , ''Snake
  , ''Player
  , ''Delayed

  , ''Level
  , ''Window
  , ''Camera
  , ''Score
  , ''Time

  , ''Pause
  , ''RandomGen
  ]

type SystemW a = SystemT World IO a
