{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Largely just a port of `purescript-behaviours` to Haskell.
module FRP.Behaviour where

import Control.Applicative ((<|>), liftA2)
import Control.Monad.Primitive (PrimMonad)
import Data.Kind (Type)
import Data.Monoid (Ap (Ap))
import FRP.Event (Event (Event))
import FRP.Event qualified as Event

-- | When we think of programming with events, we think of "reacting" to
-- notifications. With behaviours, however, we think of a value that can be
-- queried at any point in time.
--
-- I find it helpful to think of a given behaviour as representing a graph with
-- time in the X-axis and all values of the behaviour's type in the Y-axis.
-- When we sample a behaviour, we're just looking up the Y-value for the given
-- X-value.
type Behaviour ∷ (Type → Type) → Type → Type
newtype Behaviour m x
  = Behaviour { sample :: ∀ r. Event m (x → r) → Event m r }
  deriving (Semigroup, Monoid) via (Ap (Behaviour m) x)

instance Functor (Behaviour m) where
  fmap f (Behaviour xs)
    = Behaviour \k → xs $ fmap (. f) k

instance Applicative (Behaviour m) where
  Behaviour fs <*> Behaviour xs
    = Behaviour \k → xs . fs $ fmap (.) k

  liftA2 f (Behaviour xs) (Behaviour ys)
    = Behaviour \k → ys . xs $ fmap (\g x y → g (f x y)) k

  pure x = Behaviour \k → fmap ($ x) k

-- | Fold a behaviour into a second behaviour. In effect, every time a value is
-- sampled from the given behaviour, "fold" it to produce a cumulative new
-- value for the second behaviour.
fold ∷ ∀ m x y. PrimMonad m ⇒ (x → y → y) → y → Behaviour m x → Behaviour m y
fold f initial xs = Behaviour \when →
  Event \k → do
    (event, push) ← Event.create

    let behaviour ∷ Behaviour m y
        behaviour = liftA2 f xs (stepper initial event)

    input  ← Event.subscribe (sample_ behaviour when) push
    output ← Event.subscribe (Event.sample event when) k

    pure (input *> output)

-- | Sample a behaviour whenever a given event fires, ignoring the result.
sample_ ∷ Behaviour m x → Event m y → Event m x
sample_ behaviour = sample behaviour . fmap (const id)

-- | Every time the given event fires, change the value to match in the
-- returned behaviour.
stepper ∷ ∀ m x. PrimMonad m ⇒ x → Event m x → Behaviour m x
stepper initial event = Behaviour \k → Event.sample (pure initial <|> event) k

-- | Create a behaviour that tracks the value of a given monadic action.
watch ∷ Monad m ⇒ m x → Behaviour m x
watch action = Behaviour \when → Event.with action when
