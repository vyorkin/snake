module Snake.Config
  ( -- * Types
    Config(..)
  , -- * Functions
    load
    -- * Constants
  , blockSize
  ) where

import Data.Text (Text)
import Linear (V2(..))
import Control.Monad.IO.Class (MonadIO)
import Toml (TomlCodec, (.=))
import qualified Toml

import Snake.Components (Level(..))

data Config = Config
  { _configName :: !Text
    -- ^ Name of the game.
  , _configLevel :: !Level
    -- ^ Level config.
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
  <*> Toml.float "foodTTL" .= _levelFoodTTL
  <*> Toml.float "bombTTL" .= _levelBombTTL
  <*> Toml.float "holeTTL" .= _levelHoleTTL
  <*> Toml.float "wallTTL" .= _levelWallTTL

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.text "game.name" .= _configName
  <*> Toml.table levelCodec "level" .= _configLevel

blockSize :: V2 Float
blockSize = V2 64.0 64.0
