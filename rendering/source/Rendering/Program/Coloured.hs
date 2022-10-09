{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Rendering.Program.Coloured where

import Barbies qualified as B
import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL qualified as GL
import Linear ((*!!), M44, V3 (V3), axisAngle, lookAt, mkTransformation)
import Model.Raw qualified as Raw
import Rendering.Component.Camera (HasCamera (..), Camera (..))
import Rendering.Component.Project (HasProject (..))
import Rendering.Component.Transform (HasTransform (..), Transform (..))
import Rendering.Program (Compiled, compile)
import Rendering.Program qualified as Program

type Coloured :: (Type -> Type) -> Type
data Coloured f
  = Coloured
      { projectionMatrix :: f (M44 GL.GLfloat)
      , transformationMatrix :: f (M44 GL.GLfloat)
      , viewMatrix :: f (M44 GL.GLfloat)
      }
  deriving stock (Generic)
  deriving anyclass
    ( B.ApplicativeB
    , B.ConstraintsB
    , B.FunctorB
    , B.TraversableB
    )

instance Program.Renderer Coloured where
  data Model Coloured = Model { rawModel :: Raw.Model }
  toRawModel = rawModel

instance HasCamera Coloured where
  camera delta Coloured{..}
    = Coloured{ viewMatrix = fmap go delta, .. }
    where
      go Camera{..} = do
        let x = cos _cameraPitch * sin _cameraYaw
            y = sin _cameraPitch
            z = cos _cameraPitch * cos _cameraYaw

        lookAt (negate _cameraPosition + V3 x y z) (negate _cameraPosition) (V3 0 1 0)


instance HasProject Coloured where
  project matrix Coloured{..}
    = Coloured{ projectionMatrix = matrix, .. }

instance HasTransform Coloured where
  transform delta Coloured{..}
    = Coloured{ transformationMatrix = fmap go delta, .. }
    where
      go Transform{..} = _transformScale *!! do
        mkTransformation _transformRotate _transformTranslate

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
  , Program.uniforms = Coloured
      { projectionMatrix = "projection_matrix"
      , transformationMatrix = "transformation_matrix"
      , viewMatrix = "view_matrix"
      }
  }
