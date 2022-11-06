{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UnicodeSyntax #-}

module FRP.BehaviourTest where

import Control.Monad (replicateM_)
import Control.Monad.Primitive (PrimMonad)
import Data.Foldable (for_)
import FRP.Behaviour (Behaviour)
import FRP.Behaviour qualified as Behaviour
import FRP.Event qualified as Event
import FRP.EventTest (gen_events, sink)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

hprop_behaviour_sample ∷ Property
hprop_behaviour_sample = property do
  (event, push) ← Event.create
  (received, _) ← sink $ Behaviour.sample (pure 1) event

  messages ← forAll gen_events
  mapM_ (push . (+)) messages

  received === map succ messages

hprop_behaviour_fold ∷ Property
hprop_behaviour_fold = property do
  let behaviour ∷ PrimMonad m ⇒ Behaviour m Int
      behaviour = Behaviour.fold (+) 0 (pure 1)

  (event, push) ← Event.create
  (received, _) ← sink (Behaviour.sample_ behaviour event)

  count ← forAll $ Gen.int do
    Range.linear 0 100

  replicateM_ count $ push ()
  received === [ 1 .. count ]

hprop_behaviour_sample_ ∷ Property
hprop_behaviour_sample_ = property do
  (event, push) ← Event.create
  (received, _) ← sink $ Behaviour.sample_ (pure 1) event

  count ← forAll $ Gen.int (Range.linear 0 100)
  replicateM_ count $ push ()

  sum received === count

hprop_behaviour_stepper ∷ Property
hprop_behaviour_stepper = property do
  (event, push) ← Event.create
  (sampler, sample) ← Event.create

  let behaviour ∷ Behaviour (PropertyT IO) Int
      behaviour = Behaviour.stepper 0 event

  (received, _) ← sink $ Behaviour.sample_ behaviour sampler  

  messages ← forAll gen_events

  for_ messages \value → push value *> sample ()
  messages === received
