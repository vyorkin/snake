{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Snake.Components.Food.Types where

import Linear (V2(..))
import Apecs.TH (makeMapComponents)
import Control.Lens.TH (makeLenses)

data FoodType
  = Cake
  | Carrot
  | Cherry
  | Lemon
  | Orange
  | Pear
  deriving (Eq, Show, Enum, Bounded)

data Food = Food
  { _foodPos :: !(V2 Int)
  , _foodType :: !FoodType
  , _foodTimer :: !Float
  }

makeMapComponents
  [ ''Food
  ]

makeLenses ''Food
