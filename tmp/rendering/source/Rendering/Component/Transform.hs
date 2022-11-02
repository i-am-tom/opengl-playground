{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Rendering.Component.Transform where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL qualified as GL
import Linear ((*!!), M44, Quaternion, V3, mkTransformation)
import Rendering.Program (Renderer)

type Transform ∷ Type
data Transform
  = Transform
      { translate ∷ V3 GL.GLfloat
      , rotate    ∷ Quaternion GL.GLfloat
      , scale     ∷ GL.GLfloat
      }
  deriving stock (Eq, Ord, Show)

type HasTransform ∷ ((Type → Type) → Type) → Constraint
class Renderer renderer ⇒ HasTransform renderer where
  transform ∷ Functor f ⇒ f Transform → renderer f → renderer f

toMatrix ∷ Transform → M44 GL.GLfloat
toMatrix Transform{..} = scale *!! mkTransformation rotate translate
