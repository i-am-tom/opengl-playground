{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Rendering.Program.Textured where

import Barbies qualified as B
import Control.Lens (lens)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL qualified as GL
import Model.Raw qualified as Raw
import Model.Textured qualified as Textured
import Rendering.Program (Compiled, compile)
import Rendering.Program qualified as Program

type Textured ∷ (Type → Type) → Type
data Textured f
  = Textured
      {
      }
  deriving stock (Generic)
  deriving anyclass
    ( B.ApplicativeB
    , B.ConstraintsB
    , B.FunctorB
    , B.TraversableB
    )

instance Program.Renderer Textured where
  type Model Textured = Textured.Model

create ∷ IO (Compiled Textured)
create = compile Program.Configure
  { Program.attributes = Map.fromList
      [ ( "vertex_position", 0 )
      , ( "texture_coordinate", 1 )
      ]

  , Program.shaders = Map.fromList
      [ ( GL.VertexShader,   "shaders/textured/shader.vert" )
      , ( GL.FragmentShader, "shaders/textured/shader.frag" )
      ]

  , Program.textures = Set.singleton (GL.TextureUnit 0)

  , Program.setup = \model → do
      GL.activeTexture GL.$= GL.TextureUnit 0

      GL.texture        GL.Texture2D GL.$= GL.Enabled
      GL.textureBinding GL.Texture2D GL.$= Just (Textured.texture model)

  , Program.teardown = \_ → do
      GL.texture        GL.Texture2D GL.$= GL.Enabled
      GL.textureBinding GL.Texture2D GL.$= Nothing

  , Program.uniforms = Textured{}
  }
