{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}

module Model.Textured where

import Control.Lens (to)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL qualified as GL
import Model.Raw qualified as Raw

type Model ∷ Type
data Model
  = Model
      { model   ∷ Raw.Model
      , texture ∷ GL.TextureObject
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving Raw.HasRawModel via (Raw.Field "model" Model)
