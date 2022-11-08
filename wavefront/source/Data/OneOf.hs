{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- An extensible variant.
module Data.OneOf where

import Data.Kind (Constraint, Type)

-- | Like 'Either', but with any number of types in the sum.
type OneOf ∷ [Type] → Type
data OneOf xs where
  Here  ∷ x → OneOf (x ': xs)
  There ∷ OneOf xs → OneOf (x ': xs)

deriving instance All Eq xs ⇒ Eq (OneOf xs)

instance All Show xs ⇒ Show (OneOf xs) where
  show = \case
    Here  x → show x
    There x → show x

absurd ∷ OneOf '[] → x
absurd = \case

wrap ∷ x → OneOf '[x]
wrap = Here

unwrap ∷ OneOf '[x] → x
unwrap = \case
  Here  x → x
  There x → absurd x

-- | Injection into a variant.
type (∈) ∷ Type → [Type] → Constraint
class x ∈ xs where

  -- | Lift a type into a variant.
  inj ∷ x → OneOf xs

instance x ∈ (x ': xs) where
  inj = Here

instance {-# INCOHERENT #-} x ∈ xs
    ⇒ x ∈ (y ': xs) where
  inj = There . inj

-- | Projection out of a variant.
type Catch ∷ Type → [Type] → [Type] → Constraint
class Catch x xs ys | x xs -> ys where

  -- | Pluck a specific type from the variant.
  catch :: OneOf xs -> Either x (OneOf ys)

instance Catch x (x ': xs) xs where
  catch = \case
    Here  x → Left  x
    There x → Right x

instance {-# INCOHERENT #-} (Catch x xs ys, x ~ y, xs ~ ys)
    ⇒ Catch x (x ': xs) (y ': ys) where
  catch = \case
    Here  _ → error "Impossible"
    There x → fmap There (catch x)

-- | Create a constraint that applies the given constraint to all the given
-- values.
type All ∷ (Type → Constraint) → [Type] → Constraint
type family All c xs where
  All c '[     ]  = (             )
  All c (x ': xs) = (c x, All c xs)
