{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Having parsed an @OBJ@ file, what we have is a series of commands.
-- Specifically, we have vertex positions, normal vectors, texture coordinates,
-- and face triples. In order to load these objects in OpenGL, we need to
-- create arrays for positions, normals, and textures, such that the index for
-- a particular face in one is the same in another.
--
-- In other words, we can't have vertex #3 matched with normal #5: we have to
-- rearrange the lists such that their indices match. If, for example, a vertex
-- has more than one normal, then it needs to be duplicated.
module Wavefront.Aggregate where

import Data.Foldable (toList)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Monoid.Generic (GenericSemigroup (..), GenericMonoid (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector ((!?), Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Linear (V2, V3 (V3))
import Numeric.Natural (Natural)
import Wavefront.Parser (Command (..))

-- When we collect the vertices, textures, and normals while traversing the
-- list, we store them in buckets as lists. Once we have the lists, we convert
-- them to vectors for faster index lookups.
type Buckets ∷ (Type → Type) → Type
data Buckets f
  = Buckets
      { vertices ∷ f (V3 Double)
      , textures ∷ f (V2 Double)
      , normals  ∷ f (V3 Double)
      }
  deriving stock (Generic)

deriving stock instance (∀ x. Eq   x ⇒ Eq   (f x)) ⇒ Eq   (Buckets f)
deriving stock instance (∀ x. Show x ⇒ Show (f x)) ⇒ Show (Buckets f)

deriving via (GenericSemigroup (Buckets f))
  instance (∀ x. Semigroup (f x))
    ⇒ Semigroup (Buckets f)

deriving via (GenericMonoid (Buckets f))
  instance (∀ x. Monoid (f x))
    ⇒ Monoid (Buckets f)

-- | The result of successfully managing to 'aggregate' a list of commands is
-- an 'Aggregate'. This contains a series of faces represented as a triple of
-- indices. The vertex position, texture coordinate and normal vector can be
-- found by looking up this index in the three buckets.
type Aggregate ∷ Type
data Aggregate
  = Aggregate
      { buckets ∷ Buckets []
      , faces   ∷ [V3 Int]
      }
  deriving stock (Eq, Generic, Show)
  deriving Semigroup via (GenericSemigroup Aggregate)
  deriving Monoid    via (GenericMonoid    Aggregate)

-- | Aggregate a list of commands into an 'Aggregate'. Note that no error
-- handling is provided: if a triangle refers to an out-of-bounds index, we set
-- the value to zero. This is mostly performance, but also convenience: these
-- models come from files, and if they're not perfect, we should just fix the
-- files.
aggregate ∷ [Command] → Aggregate
aggregate commands = do

  -- Some laziness trickery. We build the buckets of vertices, textures, and
  -- normals in the first pass of the list, while simultaneously constructing a
  -- lazy list of faces. Once the list has been traversed, we can build the
  -- reference vectors for resolving the faces' references.
  let (triangles, buckets) = commands & foldMap \case
        GeometricVertex v → ( mempty, mempty { vertices = [ v ] } )
        TextureVertex   t → ( mempty, mempty { textures = [ t ] } )
        VertexNormal    n → ( mempty, mempty { normals  = [ n ] } )
        Triangle        f → ( [ fmap triangle f ], mempty )
          where
            triangle ∷ V3 Natural → (V3 Double, V2 Double, V3 Double)
            triangle (V3 v t n)
              = ( fromMaybe 0 (vertex  !? pred (fromIntegral v))
                , fromMaybe 0 (texture !? pred (fromIntegral t))
                , fromMaybe 0 (normal  !? pred (fromIntegral n))
                )

      -- The three reference vectors built from the "up front" list traversal.
      vertex :: Vector (V3 Double)
      vertex = Vector.fromList (vertices buckets)

      texture ∷ Vector (V2 Double)
      texture = Vector.fromList (textures buckets)

      normal ∷ Vector (V3 Double)
      normal = Vector.fromList (normals buckets)

      -- | The list of unique triples mentioned by any faces, calculated
      -- lazily /after/ the list traversal.
      nubbed ∷ Set (V3 Double, V2 Double, V3 Double)
      nubbed = Set.fromList (triangles >>= toList)

  Aggregate
    { faces   = fmap (fmap (`Set.findIndex` nubbed)) triangles
    , buckets = case unzip3 (toList nubbed) of
        (vs, ts, ns) → Buckets vs ts ns
    }
