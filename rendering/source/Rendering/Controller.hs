{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Rendering.Controller where

import Control.Exception (bracket_)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL qualified as GL
import Model.Raw qualified as Raw

prepare :: IO ()
prepare = do
  GL.clearColor GL.$= GL.Color4 1 0 0 1
  GL.clear [GL.ColorBuffer]

render :: Raw.Model -> IO ()
render model = Raw.withModel model do
  withEnabledAttributes (Map.keys (Raw.enabledAttributes model)) do
    GL.drawElements GL.Triangles (Raw.numberOfVertices model) GL.UnsignedInt nullPtr

withEnabledAttributes :: [GL.AttribLocation] -> IO x -> IO x
withEnabledAttributes locations = bracket_ setup teardown
  where
    setup :: IO ()
    setup = for_ locations \location ->
      GL.vertexAttribArray location GL.$= GL.Enabled

    teardown :: IO ()
    teardown = for_ locations \location ->
      GL.vertexAttribArray location GL.$= GL.Disabled
