{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Rendering.Program where

import Control.Cleanup (cleanup, cleanupNamedObject, withCleanup)
import Control.Exception (bracket_)
import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.Function ((&))
import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Graphics.Rendering.OpenGL qualified as GL
import Model.Loader qualified as Loader
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (hPrintf)

type Compiled :: ((Type -> Type) -> Type) -> Type
data Compiled program
  = Compiled
      { compiledProgram :: GL.Program
      }

compile
  :: Map String GL.GLuint
  -> Map GL.ShaderType FilePath
  -> program (Const GL.UniformLocation)
  -> IO (Compiled program)
compile attributes shaders uniforms = withCleanup \markForCleanup -> do
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

  attributes & Map.foldMapWithKey \name location ->
    GL.attribLocation compiledProgram name
      GL.$= GL.AttribLocation location

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
