{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Rendering.Program.Coloured where

import Barbies qualified as B
import Control.Lens ((.~), coerced, makeLenses, set)
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
import Linear ((*!!), M44, V3 (V3), axisAngle, lookAt, mkTransformation)
import Model.Raw qualified as Raw
import Rendering.Component.Camera (HasCamera (..), Camera (..))
import Rendering.Component.Camera qualified as Camera (toMatrix)
import Rendering.Component.Project (HasProject (..))
import Rendering.Component.Transform (HasTransform (..), Transform (..))
import Rendering.Component.Transform qualified as Transform
import Rendering.Program (Compiled, compile)
import Rendering.Program qualified as Program

type Coloured ∷ (Type → Type) → Type
data Coloured f
  = Coloured
      { _projectionMatrix     ∷ f (M44 GL.GLfloat)
      , _transformationMatrix ∷ f (M44 GL.GLfloat)
      , _viewMatrix           ∷ f (M44 GL.GLfloat)
      }
  deriving stock (Generic)
  deriving anyclass
    ( B.ApplicativeB
    , B.ConstraintsB
    , B.FunctorB
    , B.TraversableB
    )

makeLenses ''Coloured

instance Program.Renderer Coloured where
  type Model Coloured = Raw.Model

instance HasCamera Coloured where
  camera ∷ ∀ f. Functor f ⇒ f Camera → Coloured f → Coloured f
  camera = set viewMatrix . fmap Camera.toMatrix

instance HasProject Coloured where
  project ∷ ∀ f. Functor f ⇒ f (M44 GL.GLfloat) → Coloured f → Coloured f
  project = set projectionMatrix

instance HasTransform Coloured where
  transform ∷ ∀ f. Functor f ⇒ f Transform → Coloured f → Coloured f
  transform = set transformationMatrix . fmap Transform.toMatrix

create ∷ IO (Compiled Coloured)
create = compile Program.Configure
  { Program.attributes = Map.fromList
      [ ( "vertex_position", 0 )
      , ( "vertex_colour",   1 )
      ]

  , Program.shaders = Map.fromList
      [ ( GL.VertexShader,   "shaders/coloured/shader.vert" )
      , ( GL.FragmentShader, "shaders/coloured/shader.frag" )
      ]

  , Program.textures = Set.singleton (GL.TextureUnit 0)

  , Program.uniforms = Coloured
      { _projectionMatrix     = "projection_matrix"
      , _transformationMatrix = "transformation_matrix"
      , _viewMatrix           = "view_matrix"
      }

  , Program.setup    = mempty
  , Program.teardown = mempty
  }
