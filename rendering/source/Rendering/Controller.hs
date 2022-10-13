{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Rendering.Controller where

import Barbies (bprod, bfoldMapC)
import Barbies.Extended (bzipWithM_)
import Control.Exception (assert, bracket_)
import Control.Lens (view)
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
import Model.Raw qualified as Model
import Rendering.Delta (Delta (..))
import Rendering.Program (Compiled, Renderer (Model))
import Rendering.Program qualified as Program

prepare ∷ IO ()
prepare = do
  GL.clearColor GL.$= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]

render ∷ ∀ b. Renderer b ⇒ Compiled b → Model b → [b Delta] → IO ()
render program model@(Model.raw → inner) deltas = do
  let enabledAttributes ∷ Map GL.AttribLocation GL.VariableType
      enabledAttributes = Model.enabledAttributes inner

      requiredAttributes ∷ Map GL.AttribLocation GL.VariableType
      requiredAttributes = Program.requiredAttributes program

  assert (requiredAttributes `Map.isSubmapOf` Model.enabledAttributes inner) do
    Program.withRenderingProgram program $ Model.with inner do
      Program.renderingBracket program model do
        let numberOfVertices ∷ GL.NumArrayIndices
            numberOfVertices = Model.numberOfVertices inner

        withAttributes (Map.keys enabledAttributes) $ for_ deltas \delta → do
          let apply ∷ GL.AsUniform x ⇒ Const GL.UniformLocation x → Delta x → IO ()
              apply (Const location) = mapM_ \x → GL.asUniform x location

          bzipWithM_ @GL.AsUniform apply (Program.uniformLocations program) delta
          GL.drawElements GL.Triangles numberOfVertices GL.UnsignedInt nullPtr

withAttributes ∷ ∀ x. [GL.AttribLocation] → IO x → IO x
withAttributes locations = bracket_ setup teardown
  where
    setup ∷ IO ()
    setup = for_ locations \location →
      GL.vertexAttribArray location GL.$= GL.Enabled

    teardown ∷ IO ()
    teardown = for_ locations \location →
      GL.vertexAttribArray location GL.$= GL.Disabled
