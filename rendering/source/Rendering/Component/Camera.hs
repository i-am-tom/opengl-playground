{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Rendering.Component.Camera where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL qualified as GL
import Linear (Quaternion, V3)
import Rendering.Program (Renderer)

type Camera :: Type
data Camera
  = Camera
      { _cameraPosition :: V3 GL.GLfloat
      , _cameraPitch    :: GL.GLfloat
      , _cameraYaw      :: GL.GLfloat
      }
  deriving stock (Eq, Ord, Show)

type HasCamera :: ((Type -> Type) -> Type) -> Constraint
class Renderer renderer => HasCamera renderer where
  camera :: Functor f => f Camera -> renderer f -> renderer f
