{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Rendering.Component.Transform where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL qualified as GL
import Linear (Quaternion, V3)
import Rendering.Program (Renderer)

type Transform :: Type
data Transform
  = Transform
      { _transformTranslate :: V3 GL.GLfloat
      , _transformRotate    :: Quaternion GL.GLfloat
      , _transformScale     :: GL.GLfloat
      }
  deriving stock (Eq, Ord, Show)

type HasTransform :: ((Type -> Type) -> Type) -> Constraint
class Renderer renderer => HasTransform renderer where
  transform :: Functor f => f Transform -> renderer f -> renderer f
