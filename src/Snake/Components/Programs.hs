{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}

module Snake.Components.Programs
  ( Programs(..)
  , Key(..)
  , Compiled(..)
  , GL.Program

    -- * Initialization
  , loadAll
  , loadProgram
  , compileProgram
  , compileShader

    -- * Usage
  , withCompiled
    -- ** Lower-level
  , get
  , setUniform
  , withAttribute
  , withVertexAttribArray
  ) where

import Apecs (Component(..), SystemT, Global)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (for_)
import Data.StateVar (($=))
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector.Storable (Vector)
import Debug.Trace (traceM)
import GHC.Stack (HasCallStack)
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>))

import qualified Apecs
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector.Storable as Vector
import qualified Graphics.Rendering.OpenGL as GL

newtype Programs = Programs
  { unPrograms :: Map.Map Key Compiled
  } deriving stock (Show)
    deriving newtype (Semigroup, Monoid)

instance Component Programs where
  type Storage Programs = Global Programs

newtype Key = Key
  { unKey :: FilePath
  } deriving (Eq, Ord, Show)

data Compiled = Compiled
  { compiledProgram  :: GL.Program
  , compiledAttrs    :: Map String GL.AttribLocation
  , compiledUniforms :: Map String GL.UniformLocation -- BUG: compile-time uniforms don't work?
  } deriving (Eq, Ord, Show)

programData :: FilePath
programData = "data" </> "programs"

-- * Initialization

loadAll :: (Apecs.Has w m Programs, MonadIO m) => SystemT w m ()
loadAll = do
  programs <- Apecs.liftIO $ listDirectory programData
  for_ programs $ \progName -> do
    compiled <- Apecs.liftIO $ loadProgram progName

    traceM $ "Loaded program as " <> show progName
    traceM $ show compiled

    Apecs.modify Apecs.global $
      Programs . Map.insert (Key progName) compiled . unPrograms

loadProgram :: String -> IO Compiled
loadProgram name = do
  vertSource <- Text.readFile $ programData </> name </> "vertex" <.> "glsl"
  fragSource <- Text.readFile $ programData </> name </> "fragment" <.> "glsl"
  compileProgram vertSource fragSource

compileProgram :: Text -> Text -> IO Compiled
compileProgram vertSource fragSource = do
  vert <- compileShader GL.VertexShader vertSource
  frag <- compileShader GL.FragmentShader fragSource

  program <- GL.createProgram
  GL.attachShader program vert
  GL.attachShader program frag

  GL.linkProgram program
  linked <- GL.get $ GL.linkStatus program
  unless linked $ do
    traceM "GL.linkProgram error"
    plog <- GL.get $ GL.programInfoLog program
    traceM plog
    exitFailure

  GL.validateProgram program
  validated <- GL.get $ GL.validateStatus program
  unless validated $ do
    traceM "GL.validateProgram error"
    plog <- GL.get $ GL.programInfoLog program
    traceM plog
    exitFailure

  liftIO $ do
    GL.currentProgram $= Just program
    attrs <- GL.activeAttribs program
    uniforms <- GL.activeUniforms program -- XXX: no fragment uniforms here
    GL.currentProgram $= Nothing

    attrLocations <- for (zip [0..] attrs) $ \(ix, (_size, _type, name)) -> do
      let location = GL.AttribLocation ix
      GL.attribLocation program name $= location
      pure (name, location)

    let
      uniformLocations = Map.fromList $ do
        (ix, (_size, _type, name)) <- zip [0..] uniforms
        pure (name, GL.UniformLocation ix)

    pure Compiled
      { compiledProgram  = program
      , compiledAttrs    = Map.fromList attrLocations
      , compiledUniforms = uniformLocations
      }

compileShader :: GL.ShaderType -> Text -> IO GL.Shader
compileShader shaderType source = do
  shader <- GL.createShader shaderType
  GL.shaderSourceBS shader $= Text.encodeUtf8 source
  GL.compileShader shader
  compiled <- GL.get $ GL.compileStatus shader
  unless compiled $ do
    traceM $ "Error in " <> show shaderType
    traceM $ Text.unpack source
    exitFailure

  pure shader

-- * Usage

get
  :: ( HasCallStack
     , Apecs.Has w m Programs
     , MonadIO m
     )
  => Key
  -> Apecs.SystemT w m Compiled
get key = do
  Programs progMap <- Apecs.get Apecs.global
  case Map.lookup key progMap of
    Nothing ->
      error $ "Program not found: " <> show (unKey key)
    Just glProg ->
      pure glProg

type SetUniform w m = forall u. (Show u, GL.Uniform u) =>
  String -> u -> SystemT w m ()

type WithCompiled w m = MonadIO m =>
  SetUniform w m ->
  (String -> (GL.AttribLocation -> SystemT w m ()) -> SystemT w m ()) ->
  SystemT w m ()

withCompiled
  :: ( HasCallStack
     , Apecs.Has w m Programs
     , MonadIO m
     )
  => Key
  -> WithCompiled w m
  -> SystemT w m ()
withCompiled key action = do
  compiled <- get key
  GL.currentProgram $= Just (compiledProgram compiled)
  action (setUniform compiled) (withAttribute compiled)
  GL.currentProgram $= Nothing

setUniform
  :: (MonadIO m, GL.Uniform a)
  => Compiled
  -> String
  -> a
  -> m ()
setUniform Compiled{..} name value = do
  location <- liftIO $ GL.uniformLocation compiledProgram name
  GL.uniform location $= value

withAttribute
  :: HasCallStack
  => Compiled
  -> String
  -> (GL.AttribLocation -> m a)
  -> m a
withAttribute Compiled{compiledAttrs} name action = do
  case Map.lookup name compiledAttrs of
    Nothing ->
      error $ "unknown attribute: " <> show name
    Just location ->
      action location

withVertexAttribArray :: MonadIO m => Compiled -> String -> Vector Float -> m () -> m ()
withVertexAttribArray compiled name vertices action =
  withAttribute compiled name $ \location -> do
    liftIO . Vector.unsafeWith vertices $ \ptr ->
      GL.vertexAttribPointer location $=
        ( GL.ToFloat
        , GL.VertexArrayDescriptor 2 GL.Float 0 ptr
        )
    action
    GL.vertexAttribArray location $= GL.Disabled
