{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | A generalised 'Either' to any number of types, rather than just two.
module Data.Variant where

import Data.Bifunctor (first)
import Control.Applicative (Alternative ((<|>), empty))
import Data.Kind (Constraint, Type)
import Type.Reflection ((:~:) (..))

-- | Apply the given constraint to every item in the given list.
type All ∷ (Type → Constraint) → [Type] → Constraint
type family All c xs where
  All c '[     ]  = (             )
  All c (x ': xs) = (c x, All c xs)

-- | Evidence that a given value exists in a 'Variant'.
type Elem ∷ Type → [Type] → Type
data Elem x xs where
  Here ∷ Elem x (x ': xs)
  There ∷ Elem x xs → Elem x (y ': xs)

-- | Check whether two 'Elem' values point to the same element.
equals ∷ ∀ x y xs. Elem x xs → Elem y xs → Maybe (x :~: y)
equals  Here      Here     = Just Refl
equals (There x) (There y) = x `equals` y
equals  _         _        = Nothing

-- | We can't produce evidence of an element existing in an empty list, so it's
-- effectively isomorphic to 'Data.Void.Void'.
impossible ∷ ∀ x y. Elem x '[] → y
impossible = \case

-- | One of a list of possible types. Represented by the value and evidence
-- that its type is in the 'Variant'.
type Variant ∷ [Type] → Type
data Variant xs where
  Choice ∷ Elem x xs → x → Variant xs

instance All Eq xs ⇒ Eq (Variant xs) where
  Choice  Here     x == Choice  Here     y = x == y
  Choice (There i) x == Choice (There j) y = Choice i x == Choice j y
  Choice  _        _ == Choice  _        _ = False

instance All Show xs ⇒ Show (Variant xs) where
  show x = fold @Show x show

-- | An empty 'Variant' is isomorphic to 'Data.Void.Void'.
preposterous ∷ ∀ x. Variant '[] → x
preposterous (Choice i _) = case i of

-- | Unwrap a singleton 'Variant'.
unwrap ∷ ∀ x. Variant '[x] → x
unwrap = \case
  Choice  Here        x → x
  Choice (There void) _ → impossible void

-- | Wrap a type in a singleton 'Variant'.
wrap ∷ ∀ x. x → Variant '[x]
wrap = inject

-- | Wrap a value in a 'Variant' that contains its type.
type Inject ∷ [Type] → Type → Constraint
class Inject xs x where inject ∷ x → Variant xs

instance Inject (x ': xs) x where
  inject x = Choice Here x

instance {-# OVERLAPPABLE #-} Inject xs x ⇒ Inject (y ': xs) x where
  inject x = case inject x of Choice ix vx -> Choice (There ix) vx

-- | Attempt to wrap a value in a 'Variant', failing if the 'Variant' doesn't
-- contain its type.
type Offer ∷ [Type] → Type → Constraint
class Offer xs x where offer ∷ x → Maybe (Variant xs)

instance Offer '[] x where
  offer _ = Nothing

instance Offer (x ': xs) x where
  offer x = Just (Choice Here x)

instance {-# INCOHERENT #-} Offer xs x
    ⇒ Offer (y ': xs) x where
  offer = fmap grow . offer

-- | Pluck an item from a 'Variant'. This can be thought of as the
-- "annihilator" of 'Inject'. Consider the following:
--
--     f ∷ (Inject xs a, Inject xs b, Catch a xs ys) ⇒ Variant xs
--
-- Thanks to some INCOHERENT magic, these constraints will cancel:
--
--     f ∷ Inject xs b ⇒ Variant xs
type Catch ∷ Type → [Type] -> [Type] → Constraint
class Catch x xs ys | x xs → ys, xs ys → x, x ys → xs where
  catch ∷ Variant xs → Either (Variant ys) x

instance Catch x (x ': xs) xs where
  catch (Choice i x) = case i of
    Here    → Right x
    There j → Left (Choice j x)

instance {-# INCOHERENT #-} (y ~ z, Catch x xs ys)
    ⇒ Catch x (y ': xs) (z ': ys) where
  catch (Choice i x) = case i of
    Here    → error "Impossible"
    There j → first grow (catch (Choice j x))

-- | Begin a case match. These functions are designed to be used in the
-- following way:
--
--     given xs
--       & match \bool → f bool
--       & match \int → g int
--       & qed
given ∷ ∀ xs r. Variant xs → Either (Variant xs) r
given = Left

-- | Handle one case of a 'Variant'. See 'given' for example usage.
match ∷ ∀ x xs r. (x → r) → Either (Variant (x : xs)) r → Either (Variant xs) r
match f = either (fmap f . catch) pure

-- | Complete a case match. See 'given' for example usage.
qed ∷ ∀ r. Either (Variant '[]) r → r
qed = either preposterous id

-- | Interpret the input into the 'Variant' by attempting to build each type
-- with an 'Alternative' function.
type Interpret ∷ (Type → Constraint) → [Type] → Constraint
class Interpret c xs where
  
  -- | Interpret the input into one of the possible outputs.
  interpret ∷ ∀ f i. Alternative f ⇒ (∀ o. c o ⇒ i → f o) → i → f (Variant xs)

instance Interpret c '[] where
  interpret _ _ = empty

instance (c x, Interpret c xs) ⇒ Interpret c (x ': xs) where
  interpret f x = fmap (Choice Here) (f x) <|> fmap grow (interpret @c f x)

-- | Apply the given function to the value in the 'Variant'.
fold ∷ ∀ c xs r. All c xs ⇒ Variant xs → (∀ x. c x ⇒ x → r) → r
fold (Choice i x) f = case i of
  Here    → f x
  There j → fold @c (Choice j x) f

-- | If a value's is one of @xs@, then it's definitely also one of @x ': xs@.
-- Note that we don't need a general purpose "variant expxander" if we keep our
-- descriptions classy: @Inject Int xs ⇒ Variant xs@ can be concretised to
-- @Variant '[Int]@, but also @Variant '[String, Int]@, or
-- @Variant '[Int, ()]@, or anything else with @Int@ in it!
grow ∷ ∀ x xs. Variant xs → Variant (x ': xs)
grow (Choice i x) = Choice (There i) x

-- | Convert one 'Variant' to another. Succeeds if the underlying value's type
-- exists in both variants.
translate ∷ ∀ xs ys. All (Offer ys) xs ⇒ Variant xs → Maybe (Variant ys)
translate xs = fold @(Offer ys) xs offer
