{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Rendering.Program.Static where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Graphics.Rendering.OpenGL qualified as GL
import Rendering.Program (Compiled, compile)

type Static :: (Type -> Type) -> Type
data Static f
  = Static
      {
      }

create :: IO (Compiled Static)
create = compile attributes shaders uniforms
  where
    attributes :: Map String GL.GLuint
    attributes = Map.fromList
      [ ( "vertex_position", 0 )
      ]

    shaders :: Map GL.ShaderType FilePath
    shaders = Map.fromList
      [ ( GL.VertexShader,   "shaders/static/shader.vert" )
      , ( GL.FragmentShader, "shaders/static/shader.frag" )
      ]

    uniforms :: Static (Const GL.UniformLocation)
    uniforms = Static
      {
      }
