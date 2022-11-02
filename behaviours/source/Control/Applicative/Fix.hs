{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Applicative.Fix
  ( ApplicativeFix (afix)
  , type (∘)
  ) where

import Data.Coerce (Coercible, coerce)
import Control.Applicative (Alternative (empty))
import Data.Kind (Constraint, Type)
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Identity (Identity (Identity))

type (∘) ∷ (Type → Type) → (Type → Type) → (Type → Type)
type (∘) = Compose

type ApplicativeFix ∷ (Type → Type) → Constraint
class Applicative f ⇒ ApplicativeFix f where
  afix ∷ (∀ g. Applicative g ⇒ (f ∘ g) x → (f ∘ g) x) → f x

  default afix
    ∷ ∀ x
    . ( Alternative f
      , ∀ a b. Coercible a b ⇒ Coercible (f a) (f b)
      )
    ⇒ (∀ g. Applicative g ⇒ (f ∘ g) x → (f ∘ g) x) → f x
  afix f = coerce (f @Identity empty)
