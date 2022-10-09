{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Model.Loader where

import Control.Cleanup (Cleanup, cleanupNamedObject, withCleanup)
import Graphics.GLUtil qualified as GL (variableType)
import Control.Exception (bracket)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Traversable (for)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable)
import Foreign.Storable qualified as Storable
import GHC.TypeLits (KnownNat, natVal)
import Graphics.Rendering.OpenGL qualified as GL
import Linear (V3)
import Linear.V (Size)
import Model.Raw qualified as Raw

data Attribute where
  Attribute :: GL.AttribLocation -> [GL.GLfloat] -> GL.NumComponents -> Attribute

(~>) :: forall t. (Foldable t, KnownNat (Size t)) => GL.GLuint -> [t GL.GLfloat] -> Attribute
(~>) location values = do
  let numberOfComponents :: GL.NumComponents
      numberOfComponents = fromIntegral do
        natVal (Proxy @(Size t))

  Attribute (GL.AttribLocation location) (values >>= toList) numberOfComponents

create :: [V3 GL.GLint] -> [Attribute] -> IO Raw.Model
create indices attributes = withCleanup \markForCleanup -> do
  withVertexArrayObject \vertexArrayObject -> do
    bindIndicesBuffer (indices >>= toList) >>= markForCleanup

    enabledAttributes <-
      for attributes \(Attribute location values groupSize) -> do
        storeDataInAttributeList location groupSize values
          >>= markForCleanup

        pure (location, GL.variableType values)

    pure Raw.Model
      { vertexArrayObject = vertexArrayObject
      , numberOfVertices  = fromIntegral (length indices * 3)
      , enabledAttributes = Map.fromList enabledAttributes
      }

destroy :: Raw.Model -> IO ()
destroy = GL.deleteObjectName . Raw.vertexArrayObject

withVertexArrayObject :: (GL.VertexArrayObject -> IO x) -> IO x
withVertexArrayObject = bracket setup (const teardown)
  where
    setup :: IO GL.VertexArrayObject
    setup = do
      vertexArrayObject <- GL.genObjectName
      GL.bindVertexArrayObject GL.$= Just vertexArrayObject

      pure vertexArrayObject

    teardown :: IO ()
    teardown = GL.bindVertexArrayObject GL.$= Nothing

bindIndicesBuffer :: [GL.GLint] -> IO Cleanup
bindIndicesBuffer indices = do
  bufferObject <- GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer GL.$= Just bufferObject

  withArray indices \pointer ->
    GL.bufferData GL.ElementArrayBuffer GL.$=
      (sizeOf indices, pointer, GL.StaticDraw)

  pure (cleanupNamedObject bufferObject)

storeDataInAttributeList :: GL.AttribLocation -> GL.NumComponents -> [GL.GLfloat] -> IO Cleanup
storeDataInAttributeList location numberOfComponents information = do
  bufferObject <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer GL.$= Just bufferObject

  withArray information \pointer ->
    GL.bufferData GL.ArrayBuffer GL.$=
      (sizeOf information, pointer, GL.StaticDraw)

  GL.vertexAttribPointer location GL.$=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor numberOfComponents GL.Float 0 nullPtr
    )

  GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
  pure (cleanupNamedObject bufferObject)

sizeOf :: Storable x => [x] -> GL.GLsizeiptr
sizeOf xs = fromIntegral (Storable.sizeOf (head xs) * length xs)
