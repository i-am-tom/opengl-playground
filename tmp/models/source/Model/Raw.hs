{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Model.Raw where

import Control.Exception (bracket_)
import Control.Lens (Getter)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)
import Graphics.Rendering.OpenGL qualified as GL

type Model ∷ Type
data Model
  = Model
      { numberOfVertices ∷ GL.NumArrayIndices
      , enabledAttributes ∷ Map GL.AttribLocation GL.VariableType
      , vertexArrayObject ∷ GL.VertexArrayObject
      }
 deriving stock (Eq, Generic, Ord, Show)
 deriving anyclass (HasRawModel)

type HasRawModel ∷ Type → Constraint
class HasRawModel model where
  raw ∷ model → Model

  default raw ∷ Coercible model Model => model → Model
  raw = coerce

type Field ∷ Symbol → Type → Type
newtype Field t x = Field { unField ∷ x }

instance HasField t x Model
    => HasRawModel (Field t x) where
  raw = getField @t . unField

with ∷ ∀ x. Model → IO x → IO x
with model = bracket_ setup teardown
  where
    setup ∷ IO ()
    setup = GL.bindVertexArrayObject GL.$=! Just (vertexArrayObject model)

    teardown ∷ IO ()
    teardown = GL.bindVertexArrayObject GL.$=! Nothing
