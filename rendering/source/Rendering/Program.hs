{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Rendering.Program where

import Barbies (AllB, ApplicativeB, ConstraintsB, TraversableB (btraverse))
import Control.Exception (bracket_)
import Control.Lens (Lens')
import Control.Monad (unless)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as ByteString
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor.Const (Const (Const, getConst))
import Data.Key (forWithKey, forWithKey_)
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import Graphics.GLUtil qualified as GL (AsUniform)
import Graphics.Rendering.Binding (withBinding)
import Graphics.Rendering.OpenGL qualified as GL
import Model.Loader qualified as Loader
import Model.Raw qualified as Model
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (hPrintf)

type Renderer ∷ ((Type → Type) → Type) → Constraint
class
    ( AllB GL.AsUniform program
    , ApplicativeB program
    , ConstraintsB program
    , TraversableB program

    , Model.HasRawModel (Model program)
    ) ⇒ Renderer program where
  type Model program :: Type

type Compiled ∷ ((Type → Type) → Type) → Type
data Compiled p
  = Compiled
      { compiledProgram ∷ GL.Program
      , renderingBracket ∷ ∀ x. Model p → IO x → IO x
      , requiredAttributes ∷ Map GL.AttribLocation GL.VariableType
      , uniformLocations ∷ p (Const GL.UniformLocation)
      }

type Configure ∷ ((Type → Type) → Type) → Type
data Configure p
  = Configure
      { attributes ∷ Map String GL.GLuint
      , shaders ∷ Map GL.ShaderType FilePath
      , textures ∷ Set GL.TextureUnit
      , uniforms ∷ p (Const String)
      , setup ∷ Model p → IO ()
      , teardown ∷ Model p → IO ()
      }

compile ∷ ∀ p. TraversableB p ⇒ Configure p → IO (Compiled p)
compile Configure{..} = do
  compiledProgram ← GL.createProgram

  shaders ←
    forWithKey shaders \shaderType filepath → do
      shader ← GL.createShader shaderType
      source ← ByteString.readFile filepath

      GL.shaderSourceBS shader GL.$= source
      GL.compileShader shader

      GL.compileStatus shader >>= flip unless do
        information ← GL.shaderInfoLog shader

        hPrintf stderr "Error while compiling %s" filepath
        hPutStrLn stderr information

        exitFailure

      GL.attachShader compiledProgram shader
      pure shader

  forWithKey_ attributes \name location →
    GL.attribLocation compiledProgram name
      GL.$= GL.AttribLocation location

  GL.linkProgram compiledProgram
  GL.linkStatus compiledProgram >>= flip unless do
    information ← GL.programInfoLog compiledProgram

    hPutStrLn stderr "Error while linking program"
    hPutStrLn stderr information

    exitFailure

  for_ shaders \shader -> do
    GL.detachShader compiledProgram shader
    GL.deleteObjectName shader

  temporary ← GL.genObjectName
  GL.bindVertexArrayObject GL.$= Just temporary

  GL.validateProgram compiledProgram
  GL.validateStatus compiledProgram >>= flip unless do
    information ← GL.programInfoLog compiledProgram

    hPutStrLn stderr "Error while validating program"
    hPutStrLn stderr information

    exitFailure

  GL.bindVertexArrayObject GL.$= Nothing
  GL.deleteObjectName temporary

  requiredAttributes ← fmap Map.fromList do
    activeAttributes ← GL.get (GL.activeAttribs compiledProgram)

    for activeAttributes \(_, variableType, name) → do
      location ← GL.get (GL.attribLocation compiledProgram name)
      pure (location, variableType)

  let locate ∷ Const String x → IO (Const GL.UniformLocation x)
      locate = fmap Const . GL.get . GL.uniformLocation compiledProgram . getConst

  uniformLocations ← btraverse locate uniforms

  let renderingBracket ∷ Model p → IO x → IO x
      renderingBracket model = bracket_ (setup model) (teardown model)

  pure Compiled{..}

destroy ∷ ∀ p m. MonadIO m ⇒ Compiled p → m ()
destroy = GL.deleteObjectName . compiledProgram

withRenderingProgram ∷ ∀ p x. Compiled p → IO x → IO x
withRenderingProgram p = bracket_ setup teardown
  where
    setup ∷ IO ()
    setup = GL.currentProgram GL.$=! Just (compiledProgram p)

    teardown ∷ IO ()
    teardown = GL.currentProgram GL.$=! Nothing
