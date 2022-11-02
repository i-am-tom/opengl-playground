{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Rendering.Component.Camera where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL qualified as GL
import Linear (M44, Quaternion, V3 (V3), lookAt)
import Rendering.Program (Renderer)

type Camera ∷ Type
data Camera
  = Camera
      { position ∷ V3 GL.GLfloat
      , pitch    ∷ GL.GLfloat
      , yaw      ∷ GL.GLfloat
      }
  deriving stock (Eq, Ord, Show)

type HasCamera ∷ ((Type → Type) → Type) → Constraint
class Renderer renderer ⇒ HasCamera renderer where
  camera ∷ Functor f ⇒ f Camera → renderer f → renderer f

toMatrix ∷ Camera → M44 GL.GLfloat
toMatrix Camera{..} = do
  let rotate ∷ V3 GL.GLfloat
      rotate = V3 (cos pitch * sin yaw) (sin pitch) (cos pitch * cos yaw)

  lookAt (negate position + rotate) (negate position) (V3 0 1 0)
