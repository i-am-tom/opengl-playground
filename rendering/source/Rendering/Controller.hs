{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Rendering.Controller where

import Barbies (bprod, bfoldMapC)
import Control.Exception (assert, bracket_)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor.Const (Const (Const))
import Data.Functor.Product (Product (Pair))
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Foreign.Ptr (nullPtr)
import Graphics.GLUtil qualified as GL (AsUniform (asUniform))
import Graphics.Rendering.OpenGL qualified as GL
import Model.Raw qualified as Raw
import Rendering.Delta (Delta (..))
import Rendering.Program (Compiled, Renderer (Model, toRawModel))
import Rendering.Program qualified as Program

prepare :: IO ()
prepare = do
  GL.clearColor GL.$= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]

render :: Renderer program => Compiled program -> Model program -> [program Delta] -> IO ()
render program model@(toRawModel -> raw) deltas = do
  let enabledAttributes :: Map GL.AttribLocation GL.VariableType
      enabledAttributes = Raw.enabledAttributes raw

      requiredAttributes :: Map GL.AttribLocation GL.VariableType
      requiredAttributes = Program.requiredAttributes program

  Program.withRenderingProgram program do
    Raw.withModel raw do
      Program.renderingBracket program model do
        let numberOfVertices :: GL.NumArrayIndices
            numberOfVertices = Raw.numberOfVertices raw

        assert (requiredAttributes `Map.isSubmapOf` enabledAttributes) do
          withEnabledAttributes (Map.keys enabledAttributes) do
            for_ deltas \delta -> do
              let apply :: GL.AsUniform x => Product Delta (Const GL.UniformLocation) x -> IO ()
                  apply (Pair delta (Const location)) = case delta of
                    Changed x -> GL.asUniform x location
                    Unchanged -> pure ()

              bfoldMapC @GL.AsUniform apply (bprod delta (Program.uniformLocations program))
              GL.drawElements GL.Triangles numberOfVertices GL.UnsignedInt nullPtr

withEnabledAttributes :: [GL.AttribLocation] -> IO x -> IO x
withEnabledAttributes locations = bracket_ setup teardown
  where
    setup :: IO ()
    setup = for_ locations \location ->
      GL.vertexAttribArray location GL.$= GL.Enabled

    teardown :: IO ()
    teardown = for_ locations \location ->
      GL.vertexAttribArray location GL.$= GL.Disabled
