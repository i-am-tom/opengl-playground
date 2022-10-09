{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Rendering.Program where

import Barbies (AllB, ApplicativeB, ConstraintsB, TraversableB (btraverse))
import Control.Cleanup (cleanup, cleanupNamedObject, withCleanup)
import Control.Exception (bracket_)
import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.Function ((&))
import Data.Functor.Const (Const (Const, getConst))
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Traversable (for)
import Graphics.GLUtil qualified as GL (AsUniform)
import Graphics.Rendering.OpenGL qualified as GL
import Model.Loader qualified as Loader
import Model.Raw qualified as Raw
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (hPrintf)

type Renderer :: ((Type -> Type) -> Type) -> Constraint
class
    ( AllB GL.AsUniform program
    , ApplicativeB program
    , ConstraintsB program
    , TraversableB program
    ) => Renderer program where
  data Model program :: Type

  toRawModel :: Model program -> Raw.Model

type Compiled :: ((Type -> Type) -> Type) -> Type
data Compiled program
  = Compiled
      { compiledProgram    :: GL.Program
      , requiredAttributes :: Map GL.AttribLocation GL.VariableType
      , renderingBracket   :: forall x. Model program -> IO x -> IO x
      , uniformLocations   :: program (Const GL.UniformLocation)
      }

type Configure :: ((Type -> Type) -> Type) -> Type
data Configure program
  = Configure
      { attributes :: Map String GL.GLuint
      , shaders    :: Map GL.ShaderType FilePath
      , textures   :: Set GL.TextureUnit
      , uniforms   :: program (Const String)
      , setup      :: Model program -> IO ()
      , teardown   :: Model program -> IO ()
      }

compile :: forall program. TraversableB program => Configure program -> IO (Compiled program)
compile Configure{..} = withCleanup \markForCleanup -> do
  compiledProgram <- GL.createProgram

  shaders & Map.foldMapWithKey \shaderType filepath -> do
    shader <- GL.createShader shaderType
    source <- ByteString.readFile filepath

    GL.shaderSourceBS shader GL.$= source
    GL.compileShader shader

    GL.compileStatus shader >>= flip unless do
      information <- GL.shaderInfoLog shader

      hPrintf stderr "Error while compiling %s" filepath
      hPutStrLn stderr information

      exitFailure

    GL.attachShader compiledProgram shader
    markForCleanup $ cleanup do
      GL.detachShader compiledProgram shader
      GL.deleteObjectName shader

  attributes & Map.foldMapWithKey \name location ->
    GL.attribLocation compiledProgram name
      GL.$= GL.AttribLocation location

  Loader.withVertexArrayObject \vertexArrayObject -> do
    markForCleanup (cleanupNamedObject vertexArrayObject)

    GL.linkProgram compiledProgram
    GL.linkStatus compiledProgram >>= flip unless do
      information <- GL.programInfoLog compiledProgram

      hPutStrLn stderr "Error while linking program"
      hPutStrLn stderr information

      exitFailure

    GL.validateProgram compiledProgram
    GL.validateStatus compiledProgram >>= flip unless do
      information <- GL.programInfoLog compiledProgram

      hPutStrLn stderr "Error while validating program"
      hPutStrLn stderr information

      exitFailure

  requiredAttributes <- fmap Map.fromList do
    activeAttributes <- GL.activeAttribs compiledProgram

    for activeAttributes \(_, variableType, name) -> do
      location <- GL.get (GL.attribLocation compiledProgram name)
      pure (location, variableType)

  let locate :: Const String x -> IO (Const GL.UniformLocation x)
      locate = fmap Const . GL.get . GL.uniformLocation compiledProgram . getConst

  uniformLocations <- btraverse locate uniforms

  let renderingBracket :: Model program -> IO x -> IO x
      renderingBracket model = bracket_ (setup model) (teardown model)

  pure Compiled{..}

destroy :: Compiled program -> IO ()
destroy = GL.deleteObjectName . compiledProgram

withRenderingProgram :: Compiled program -> IO x -> IO x
withRenderingProgram Compiled{ compiledProgram } = bracket_ setup teardown
  where
    setup :: IO ()
    setup = GL.currentProgram GL.$= Just compiledProgram

    teardown :: IO ()
    teardown = GL.currentProgram GL.$= Nothing
