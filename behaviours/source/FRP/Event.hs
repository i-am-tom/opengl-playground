{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Largely just a port of `purescript-event` to Haskell.
module FRP.Event where

import Control.Applicative (Alternative ((<|>), empty), liftA2)
import Control.Arrow ((&&&))
import Control.Monad (join)
import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap (delete, empty, insert)
import Data.Kind (Type)
import Data.Primitive.MutVar (MutVar, atomicModifyMutVar', newMutVar, readMutVar, writeMutVar)
import Witherable (Filterable (mapMaybe))

-- | An event is a function that will "register" the given listener and return
-- an action to unregister.
type Event ∷ (Type → Type) → (Type → Type)
newtype Event m x = Event { subscribe ∷ (x → m ()) → m (m ()) }
  deriving stock Functor

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

-- | Create a new custom event. The result is the event, and a function to pass
-- the given value to all registered listeners.
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

-- | Fold over an event stream. Works exactly the same way as 'scanl': every
-- time the given event is triggered, its value is combined with the
-- accumulated total to produce a "cumulative" stream in the returned event.
fold ∷ ∀ m x y. PrimMonad m ⇒ (x → y → y) → y → Event m x → Event m y
fold f initial xs = Event \k → do
  acc ← newMutVar initial

  subscribe xs \x → join do
    atomicModifyMutVar' acc do
      f x &&& k . f x

-- | Register to the given stream of streams. Every time a stream is returned,
-- unsubscribe to the previous stream and begin piping the new stream's events
-- to the output.
latest ∷ ∀ m x. PrimMonad m ⇒ Event m (Event m x) → Event m x
latest xs = Event \k → do
  inner ← newMutVar $ pure ()
  outer ← subscribe xs \event → do
    join (readMutVar inner)
    subscribe event k >>= writeMutVar inner

  pure do
    join (readMutVar inner)
    outer

-- | Every time the /second/ event fires, the last value from the /first/ event
-- will be applied to the function to produce the result.
sample ∷ ∀ m x y. PrimMonad m ⇒ Event m x → Event m (x → y) → Event m y
sample xs fs = Event \k → do
  ref ← newMutVar Nothing

  cx ← subscribe xs \x →
    writeMutVar ref (Just x)

  cf ← subscribe fs \f →
    readMutVar ref >>= \mx →
      mapM_ (k . f) mx

  pure (cx *> cf)

-- | Sample a monadic action every time the event fires.
with ∷ Monad m ⇒ m x → Event m (x → y) → Event m y
with xs e = Event \k → subscribe e \f → xs >>= k . f
