{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Rendering.Program.Textured where

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

type Textured :: (Type -> Type) -> Type
data Textured f
  = Textured
      {
      }

instance Program.Renderer Textured where
  data Model Textured
    = Model
        { rawModel :: Raw.Model
        , texture  :: GL.TextureObject
        }

  toRawModel = rawModel

create :: IO (Compiled Textured)
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

  , Program.setup = \model -> do
      GL.activeTexture GL.$= GL.TextureUnit 0

      GL.texture        GL.Texture2D GL.$= GL.Enabled
      GL.textureBinding GL.Texture2D GL.$= Just (texture model)

  , Program.teardown = \_ -> do
      GL.texture        GL.Texture2D GL.$= GL.Enabled
      GL.textureBinding GL.Texture2D GL.$= Nothing

  , Program.uniforms = Textured{}
  }
