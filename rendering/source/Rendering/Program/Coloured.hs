{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Rendering.Program.Coloured where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Graphics.Rendering.OpenGL qualified as GL
import Model.Raw qualified as Raw
import Rendering.Program (Compiled, compile)
import Rendering.Program qualified as Program

type Coloured :: (Type -> Type) -> Type
data Coloured f
  = Coloured
      {
      }

instance Program.Renderer Coloured where
  data Model Coloured = Model { rawModel :: Raw.Model }
  toRawModel = rawModel

create :: IO (Compiled Coloured)
create = compile Program.Configure
  { Program.attributes = Map.fromList
      [ ( "vertex_position", 0 )
      , ( "vertex_colour", 1 )
      ]

  , Program.shaders = Map.fromList
      [ ( GL.VertexShader,   "shaders/coloured/shader.vert" )
      , ( GL.FragmentShader, "shaders/coloured/shader.frag" )
      ]

  , Program.textures = Set.singleton (GL.TextureUnit 0)

  , Program.setup    = mempty
  , Program.teardown = mempty
  , Program.uniforms = Coloured{}
  }
