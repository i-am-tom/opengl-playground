{-# OPTIONS_GHC -Wall -Wextra #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}

module FRP.Event where

import Control.Applicative (Alternative ((<|>), empty), liftA2)
import Control.Applicative.Fix (ApplicativeFix)
import Control.Arrow ((&&&))
import Control.Monad (join)
import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Primitive.MutVar
import Witherable (Filterable (mapMaybe))

type Event ∷ (Type → Type) → (Type → Type)
newtype Event m x = Event { subscribe ∷ (x → m ()) → m (m ()) }
  deriving anyclass (ApplicativeFix)
  deriving stock (Functor)

instance Applicative m ⇒ Filterable (Event m) where
  mapMaybe f xs = Event \k → subscribe xs (traverse_ k . f)

instance PrimMonad m ⇒ Applicative (Event m) where
  Event fs <*> Event xs = Event \k → do
    ref ← newMutVar (Nothing, Nothing)

    cf <- fs \f → join do
      atomicModifyMutVar' ref \(_, mx) →
        ( ( Just f, mx ), mapM_ k (Just f <*> mx) )

    cx <- xs \x → join do
      atomicModifyMutVar' ref \(mf, _) →
        ( ( mf, Just x ), mapM_ k (mf <*> Just x) )

    pure (cf *> cx)

  pure x = Event \k → pure () <$ k x

instance PrimMonad m ⇒ Alternative (Event m) where
  Event xs <|> Event ys = Event \k → liftA2 (*>) (xs k) (ys k)
  empty = Event \_ → pure (pure ())

create ∷ ∀ m x. PrimMonad m ⇒ m (Event m x, x → m ())
create = do
  subscribers ← newMutVar IntMap.empty
  identifier  ← newMutVar 0

  let update ∷ ∀ s. MutVar (PrimState m) s → (s → s) → m ()
      update ref f = atomicModifyMutVar' ref \x → (f x, ())

      event ∷ Event m x
      event = Event \k → do
        next ← atomicModifyMutVar' identifier \x → (succ x, x)

        update subscribers (IntMap.insert next k)
        pure $ update subscribers (IntMap.delete next)

      push ∷ x -> m ()
      push x = readMutVar subscribers >>= mapM_ \k → k x

  pure (event, push)

newest ∷ PrimMonad m ⇒ Event m x → m (MutVar (PrimState m) (Maybe x), m ())
newest xs = do
  ref ← newMutVar Nothing

  cancel ← subscribe xs \x ->
    writeMutVar ref (Just x)

  pure (ref, cancel)

sample ∷ ∀ m x y. PrimMonad m ⇒ Event m x → Event m (x → y) → Event m y
sample xs fs = Event \k → do
  (ref, cx) ← newest xs

  cf ← subscribe fs \f →
    readMutVar ref >>= \mx →
      mapM_ (k . f) mx

  pure (cx *> cf)

fold ∷ ∀ m x y. PrimMonad m ⇒ (x → y → y) → y → Event m x → Event m y
fold f initial xs = Event \k → do
  acc ← newMutVar initial

  subscribe xs \x → join do
    atomicModifyMutVar' acc do
      f x &&& k . f x

once ∷ ∀ m x. PrimMonad m ⇒ Event m x → (x → m ()) → m (m ())
once event k = do
  ref ← newMutVar $ pure ()

  cancel ← subscribe event \x →
    k x *> join (readMutVar ref)

  writeMutVar ref cancel
  pure cancel