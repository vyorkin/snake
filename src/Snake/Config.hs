module Snake.Config
  ( -- * Types
    Config(..)
  , Level(..)
  , -- * Functions
    load
    -- * Constants
  , cellSize
  ) where

import Data.Text (Text)
import Linear (V2(..))
import Control.Monad.IO.Class (MonadIO)
import Toml (TomlCodec, (.=))
import qualified Toml

data Config = Config
  { _configName :: !Text
    -- ^ Name of the game.
  , _configLevel :: !Level
    -- ^ Level config.
  } deriving (Show)

data Level = Level
  { _levelWidth :: !Int
    -- ^ Game field width.
  , _levelHeight :: !Int
    -- ^ Game field height.
  , _levelSnakeSpeed :: !Float
    -- ^ Initial speed of the Snake.
  , _levelFoodSpawn :: !Int
    -- ^ Amount of food to spawn at a time.
  , _levelFoodTTL :: !Int
    -- ^ Food lifetime.
  } deriving (Show)

-- | Loads the @config.toml@ file.
load :: MonadIO m => m Config
load = Toml.decodeFile configCodec "config.toml"

levelCodec :: TomlCodec Level
levelCodec = Level
  <$> Toml.int "width" .= _levelWidth
  <*> Toml.int "height" .= _levelHeight
  <*> Toml.float "snakeSpeed" .= _levelSnakeSpeed
  <*> Toml.int "foodSpawn" .= _levelFoodSpawn
  <*> Toml.int "foodTTL" .= _levelFoodTTL

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.text "game.name" .= _configName
  <*> Toml.table levelCodec "level" .= _configLevel

cellSize :: V2 Float
cellSize = V2 64.0 64.0
