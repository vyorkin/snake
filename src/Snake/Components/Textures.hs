{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

module Snake.Components.Textures
  ( Textures(..)
  , Key(..)
  , Texture(..)

    -- * Initialization
  , loadAll
  , loadTexture

    -- * Usage
  , get
  , toTextureKey
  ) where

import qualified Data.Char as Char (toLower)
import Apecs (Component(..), SystemT, Global)
import Control.Exception (bracket, bracket_)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.StateVar (($=))
import Debug.Trace (traceM)
import Foreign (Int32, Ptr)
import GHC.Stack (HasCallStack)
import Linear (V2(..))
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), dropExtension, makeRelative)

import qualified Apecs
import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import qualified SDL.Image

newtype Textures = Textures
  { unTextures :: Map Key Texture
  } deriving stock (Show)
    deriving newtype (Semigroup, Monoid)

instance Component Textures where
  type Storage Textures = Global Textures

-- TODO: generate Yesod-style static keys
newtype Key = Key
  { unKey :: FilePath
  } deriving (Eq, Ord, Show)

data Texture = Texture
  { textureSource :: FilePath
  , textureWidth  :: Int32
  , textureHeight :: Int32
  , textureObject :: GL.TextureObject
  } deriving (Eq, Ord, Show)

-- * Initialization

textureData :: FilePath
textureData = "data" </> "textures"

loadAll :: (Apecs.Has w m Textures, MonadIO m) => SystemT w m ()
loadAll = loadFrom textureData

loadFrom :: (Apecs.Has w m Textures, MonadIO m) => FilePath -> SystemT w m ()
loadFrom current = do
  names <- Apecs.liftIO $ listDirectory current
  for_ names $ \name -> do
    let next = current </> name
    isDir <- Apecs.liftIO $ doesDirectoryExist next
    if isDir then
      loadFrom next
    else do
      let textureId = Key . dropExtension $ makeRelative textureData next
      texture <- Apecs.liftIO $ loadTexture (GL.Linear', GL.Repeated, GL.Repeat) next
      traceM $ "Loaded texture as " <> show (unKey textureId)
      traceM $ show texture
      Apecs.modify Apecs.global $
        Textures . Map.insert textureId texture . unTextures

loadTexture :: (GL.TextureFilter, GL.Repetition, GL.Clamping) -> FilePath -> IO Texture
loadTexture (glFilter, glRepeat, glClamp) source = do
  [tex] <- GL.genObjectNames 1

  loadImage source $ \imgWidth imgHeight imgPtr -> do
    let target = GL.Texture2D
    GL.textureBinding  target      $= Just tex
    GL.texture         target      $= GL.Enabled
    GL.textureFilter   target      $= ((glFilter, Nothing), glFilter)
    GL.textureWrapMode target GL.S $= (glRepeat, glClamp)
    GL.textureWrapMode target GL.T $= (glRepeat, glClamp)
    GL.generateMipmap  target      $= GL.Enabled

    GL.texImage2D
      GL.Texture2D
      GL.NoProxy
      0
      GL.RGBA8
      (GL.TextureSize2D imgWidth imgHeight)
      0
      (GL.PixelData GL.ABGR GL.UnsignedByte imgPtr)

    GL.textureFilter  target $= ((glFilter, Just glFilter), glFilter)
    GL.textureBinding target $= Nothing

    pure Texture
      { textureSource = source
      , textureWidth  = imgWidth
      , textureHeight = imgHeight
      , textureObject = tex
      }

loadImage :: Integral size => FilePath -> (size -> size -> Ptr () -> IO a) -> IO a
loadImage filePath action =
  bracket (SDL.Image.load filePath) SDL.freeSurface $ \source -> do
    V2 width height <- SDL.surfaceDimensions source
    let sdlSize = V2 width height
    bracket (SDL.createRGBSurface sdlSize SDL.RGBA8888) SDL.freeSurface $ \temporary -> do
      _nothing <- SDL.surfaceBlit source Nothing temporary Nothing
      bracket_ (SDL.lockSurface temporary) (SDL.unlockSurface temporary) $ do
        pixels <- SDL.surfacePixels temporary
        action (fromIntegral width) (fromIntegral height) pixels

-- * Usage

get
  :: ( HasCallStack
     , Apecs.Has w m Textures
     , MonadIO m
     )
  => Key
  -> Apecs.SystemT w m Texture
get key = do
  Textures textureMap <- Apecs.get Apecs.global
  case Map.lookup key textureMap of
    Nothing ->
      error $ "Texture not found: " <> show (unKey key)
    Just texture ->
      pure texture

toTextureKey :: Show a => FilePath -> a -> Key
toTextureKey folder = Key . (folder </>) . lowercase . show
  where
    lowercase (c:cs) = Char.toLower c : cs
    lowercase [] = []
