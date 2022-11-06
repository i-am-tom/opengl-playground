{-# OPTIONS_GHC -Wall -Wextra #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module FRP.EventTest where

import Control.Applicative (Alternative ((<|>), empty))
import Control.Monad ((<=<), guard, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Primitive.MutVar (atomicModifyMutVar', newMutVar, readMutVar)
import FRP.Event (Event)
import FRP.Event qualified as Event
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.IO.Unsafe (unsafeInterleaveIO)
import Witherable (mapMaybe)

sink ∷ ∀ m x. MonadIO m ⇒ Event m x → m ([x], m ())
sink event = do
  ref ← liftIO $ newMutVar []

  cancel ←
    Event.subscribe event \x → liftIO do
      atomicModifyMutVar' ref \xs →
        (xs ++ [x], ())

  results ← liftIO do
    unsafeInterleaveIO do
      readMutVar ref

  pure (results, cancel)

gen_events ∷ Gen [Int]
gen_events = do
  Gen.list (Range.linear 0 100) do
    Gen.enumBounded

hprop_event_push_pull ∷ Property
hprop_event_push_pull = property do
  (event, push) ← Event.create
  (received, _) ← sink event

  expected ← forAll gen_events
  mapM_ push expected

  received === expected

hprop_event_unsubscribe ∷ Property
hprop_event_unsubscribe = property do
  (event, push) ← Event.create
  (received, cancel) ← sink event

  before ← forAll gen_events
  after  ← forAll gen_events

  mapM_ push before
  cancel
  mapM_ push after

  received === before

hprop_event_sample ∷ Property
hprop_event_sample = property do
  (sampled, push) ← Event.create
  (sampler, sample) ← Event.create

  ref ← newMutVar 0

  _ ← Event.subscribe (Event.sample sampled sampler) \n ->
    atomicModifyMutVar' ref \_ → (n, ())

  forAll gen_events >>= mapM_ \actual -> do
    push actual

    forAll Gen.bool >>= flip when do
      sample id

      expected ← readMutVar ref
      expected === actual

hprop_event_fold ∷ Property
hprop_event_fold = property do
  (event, push) ← Event.create
  (received, _) ← sink $ Event.fold (+) 0 event

  messages ← forAll gen_events
  mapM_ push messages

  received === tail do
    scanl (+) 0 messages

hprop_event_with ∷ Property
hprop_event_with = property do
  (event, push) ← Event.create

  messages ← forAll gen_events
  ref ← newMutVar messages

  let shift ∷ PropertyT IO Int
      shift = atomicModifyMutVar' ref \case
        x : xs → (xs, succ x)
        [    ] → error "List exhausted too quickly?"

  (received, _) ← sink $ Event.with shift (fmap (,) event)

  mapM_ push messages
  received === zip messages (map succ messages)

-- * @Functor@

hprop_event_functor_identity ∷ Property
hprop_event_functor_identity = property do
  (event, push) ← Event.create
  (received, _) ← sink (fmap id event)

  expected ← forAll gen_events
  mapM_ push expected

  received === expected

hprop_event_functor_composition ∷ Property
hprop_event_functor_composition = property do
  modulo ← forAll Gen.enumBounded
  scaler ← forAll Gen.enumBounded

  let f ∷ Int → Int
      f x = x `mod` modulo

      g ∷ Int → Int
      g x = x * scaler

  (event, push) ← Event.create

  (these, _) ← sink $ fmap (f . g) event
  (those, _) ← sink $ fmap f (fmap g event)

  forAll gen_events >>= mapM_ push
  these === those

-- * @Filterable@

hprop_event_filterable_conservation ∷ Property
hprop_event_filterable_conservation = property do
  modulo ← forAll Gen.enumBounded

  let f ∷ Int → Int
      f x = x `mod` modulo

  (event, push) ← Event.create

  (these, _) ← sink $ mapMaybe (Just . f) event
  (those, _) ← sink $ fmap f event

  forAll gen_events >>= mapM_ push
  these === those

hprop_event_filterable_composition ∷ Property
hprop_event_filterable_composition = property do
  threshold  ← forAll Gen.enumBounded
  multiplier ← forAll Gen.enumBounded

  let f ∷ Int → Maybe Int
      f x = x <$ guard (x > threshold)

      g ∷ Int → Maybe Int
      g x = x <$ guard (mod x multiplier == 0)

  (event, push) ← Event.create

  (these, _) ← sink $ mapMaybe f (mapMaybe g event)
  (those, _) ← sink $ mapMaybe (f <=< g) event

  forAll gen_events >>= mapM_ push
  these === those

-- * @Applicative@

hprop_event_applicative_identity ∷ Property
hprop_event_applicative_identity = property do
  (event, push) ← Event.create

  (these, _) ← sink $ pure id <*> event
  (those, _) ← sink event

  forAll gen_events >>= mapM_ push
  these === those

type Three ∷ Type → Type
data Three a = X a | Y a | Z a
  deriving stock (Eq, Ord, Show)
  deriving stock (Functor, Foldable, Traversable)

hprop_event_applicative_composition ∷ Property
hprop_event_applicative_composition = property do
  (u, pu) ← Event.create
  (v, pv) ← Event.create
  (w, pw) ← Event.create

  (these, _) ← sink $ pure (.) <*> u <*>  v <*> w
  (those, _) ← sink $              u <*> (v <*> w)

  commands ∷ [Three Int] ← forAll do
    Gen.list (Range.linear 0 100) do
      Gen.choice
        [ fmap X Gen.enumBounded
        , fmap Y Gen.enumBounded
        , fmap Z Gen.enumBounded
        ]

  for_ commands \case
    X x → pu (+ x)
    Y y → pv (* y)
    Z z → pw    z
  
  these === those

hprop_event_applicative_homomorphism ∷ Property
hprop_event_applicative_homomorphism = property do
  f ← fmap (+) $ forAll $ Gen.enumBounded @_ @Int
  x ←            forAll $ Gen.enumBounded @_ @Int

  (these, _) ← sink $ pure f <*> pure x
  (those, _) ← sink $ pure (f x)

  these === those

hprop_event_applicative_interchange ∷ Property
hprop_event_applicative_interchange = property do
  x ← forAll Gen.enumBounded

  (event, push) ← Event.create

  (these, _) ← sink $ event <*> pure x
  (those, _) ← sink $ pure ($ x) <*> event

  forAll gen_events >>= mapM_ \y → push \z → y + z
  these === those

-- * @Alternative@

hprop_event_alternative_identity ∷ Property
hprop_event_alternative_identity = property do
  (event, push) ← Event.create

  (these, _) ← sink $ event <|> empty
  (those, _) ← sink $ event

  forAll gen_events >>= mapM_ push
  these === those

hprop_event_alternative_associativity ∷ Property
hprop_event_alternative_associativity = property do
  (ex, px) ← Event.create
  (ey, py) ← Event.create
  (ez, pz) ← Event.create

  (these, _) ← sink $ ex <|> (ey <|> ez)
  (those, _) ← sink $ (ex <|> ey) <|> ez

  commands ∷ [Three Int] ← forAll do
    Gen.list (Range.linear 0 100) do
      Gen.choice
        [ fmap X Gen.enumBounded
        , fmap Y Gen.enumBounded
        , fmap Z Gen.enumBounded
        ]

  for_ commands \case
    X x → px x
    Y y → py y
    Z z → pz z
  
  these === those

hprop_event_alternative_commutativity ∷ Property
hprop_event_alternative_commutativity = property do
  (ex, px) ← Event.create
  (ey, py) ← Event.create

  (these, _) ← sink $ ex <|> ey
  (those, _) ← sink $ ey <|> ex

  commands ∷ [Either Int Int] ← forAll do
    Gen.list (Range.linear 0 100) do
      Gen.either Gen.enumBounded Gen.enumBounded

  for_ commands \case
    Left  x → px x
    Right y → py y
  
  these === those
