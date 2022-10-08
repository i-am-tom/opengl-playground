{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Loader where

import Control.Exception (bracket)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable)
import Foreign.Storable qualified as Storable
import Graphics.Rendering.OpenGL qualified as GL
import Linear (V3)
import Model.Raw qualified as Raw
import Control.Cleanup (Cleanup, cleanup, withCleanup)

create :: [V3 GL.GLfloat] -> IO Raw.Model
create positions = withCleanup \markForCleanup -> do
  withVertexArrayObject \vertexArrayObject -> do
    let location :: GL.AttribLocation
        location = GL.AttribLocation 0

        floats :: [GL.GLfloat]
        floats = positions >>= toList

    storeDataInAttributeList location floats
      >>= markForCleanup

    pure Raw.Model
      { vertexArrayObject = vertexArrayObject
      , numberOfVertices  = fromIntegral (length positions)
      , enabledAttributes = Map.singleton location GL.FloatVec3
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

storeDataInAttributeList :: GL.AttribLocation -> [GL.GLfloat] -> IO Cleanup
storeDataInAttributeList location information = do
  bufferObject <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer GL.$= Just bufferObject

  withArray information \pointer ->
    GL.bufferData GL.ArrayBuffer GL.$=
      (sizeOf information, pointer, GL.StaticDraw)

  GL.vertexAttribPointer location GL.$=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)

  GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
  pure $ cleanup (GL.deleteObjectName bufferObject)

sizeOf :: [GL.GLfloat] -> GL.GLsizeiptr
sizeOf xs = fromIntegral (Storable.sizeOf (head xs) * length xs)
