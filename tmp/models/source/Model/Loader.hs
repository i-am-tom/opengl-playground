{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module Model.Loader where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Traversable (for)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable)
import Foreign.Storable qualified as Storable
import GHC.TypeLits (KnownNat, natVal)
import Graphics.GLUtil qualified as GL (variableType)
import Graphics.Rendering.OpenGL qualified as GL
import Linear (V3)
import Linear.V (Size)
import Model.Raw qualified as Raw

type Attribute ∷ Type
data Attribute where
  Attribute ∷ GL.AttribLocation → [GL.GLfloat] → GL.NumComponents → Attribute

(~>) ∷ ∀ t. (Foldable t, KnownNat (Size t)) ⇒ GL.GLuint → [t GL.GLfloat] → Attribute
(~>) location values = do
  let numberOfComponents ∷ GL.NumComponents
      numberOfComponents = fromIntegral $ natVal @(Size t) undefined

  Attribute (GL.AttribLocation location) (values >>= toList) numberOfComponents

create ∷ ∀ m. [V3 GL.GLint] → [Attribute] → IO Raw.Model
create indices attributes = do
  vertexArrayObject ← GL.genObjectName
  GL.bindVertexArrayObject GL.$= Just vertexArrayObject

  bufferObject ← GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer GL.$= Just bufferObject

  withArray indices \pointer →
    GL.bufferData GL.ElementArrayBuffer GL.$=
      (sizeOf indices, pointer, GL.StaticDraw)

  enabledAttributes ← fmap Map.fromList do
    for attributes \(Attribute location values numberOfComponents) → do
      bufferObject ← GL.genObjectName
      GL.bindBuffer GL.ArrayBuffer GL.$= Just bufferObject

      withArray values \pointer →
        GL.bufferData GL.ArrayBuffer GL.$=
          (sizeOf values, pointer, GL.StaticDraw)

      GL.vertexAttribPointer location GL.$=
        ( GL.ToFloat
        , GL.VertexArrayDescriptor numberOfComponents GL.Float 0 nullPtr
        )

      pure (bufferObject, (location, GL.variableType values))

  GL.bindVertexArrayObject GL.$= Nothing
  GL.bindBuffer GL.ElementArrayBuffer GL.$= Nothing
  GL.deleteObjectName bufferObject

  pure Raw.Model
    { vertexArrayObject = vertexArrayObject
    , numberOfVertices  = fromIntegral (length indices * 3)
    , enabledAttributes = Map.fromList (Map.elems enabledAttributes)
    }

destroy ∷ ∀ m. MonadIO m ⇒ Raw.Model → m ()
destroy = GL.deleteObjectName . Raw.vertexArrayObject

sizeOf ∷ ∀ x. Storable x ⇒ [x] → GL.GLsizeiptr
sizeOf xs = fromIntegral (Storable.sizeOf (head xs) * length xs)
