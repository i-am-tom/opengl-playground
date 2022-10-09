{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Rendering.Component.Project where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL qualified as GL
import Linear (M44)
import Rendering.Program (Renderer)

type HasProject :: ((Type -> Type) -> Type) -> Constraint
class Renderer renderer => HasProject renderer where
  project :: Functor f => f (M44 GL.GLfloat) -> renderer f -> renderer f
