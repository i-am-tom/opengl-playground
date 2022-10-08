{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Model.Raw where

import Control.Exception (bracket_)
import Data.Map.Strict (Map)
import Graphics.Rendering.OpenGL qualified as GL

data Model
  = Model
      { vertexArrayObject :: GL.VertexArrayObject
      , numberOfVertices  :: GL.NumArrayIndices
      , enabledAttributes :: Map GL.AttribLocation GL.VariableType
      }

withModel :: Model -> IO x -> IO x
withModel model = bracket_ setup teardown
  where
    setup :: IO ()
    setup = GL.bindVertexArrayObject GL.$= Just (vertexArrayObject model)

    teardown :: IO ()
    teardown = GL.bindVertexArrayObject GL.$= Nothing
