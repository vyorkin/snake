module Snake.Config
  ( -- * Types
    Config(..)
  , Level(..)
  , -- * Functions
    load
  ) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Toml (TomlCodec, (.=))
import qualified Toml

data Config = Config
  { _name :: !Text
  , _level :: !Level
  } deriving (Show)

data Level = Level
  { _width :: !Int
  , _height :: !Int
  , _speed :: !Float
  } deriving (Show)

-- | Loads the @config.toml@ file.
load :: MonadIO m => m Config
load = Toml.decodeFile configCodec "config.toml"

levelCodec :: TomlCodec Level
levelCodec = Level
  <$> Toml.int "width" .= _width
  <*> Toml.int "height" .= _height
  <*> Toml.float "speed" .= _speed

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.text "game.name" .= _name
  <*> Toml.table levelCodec "level" .= _level
