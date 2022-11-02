{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Rendering.Delta where

import Barbies qualified as B
import Data.Kind (Type)

type Delta :: Type -> Type
data Delta x = Unchanged | Changed x
  deriving (Eq, Foldable, Functor, Ord, Show)

instance Semigroup (Delta x) where
  this <> Unchanged = this
  ____ <> Changed x = Changed x

instance Monoid (Delta x) where
  mempty = Unchanged

bunchanged :: B.ApplicativeB b => b Delta
bunchanged = B.bpure Unchanged
