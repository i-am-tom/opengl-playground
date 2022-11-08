{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.OneOfTest where

import Data.OneOf
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

gen_nested_either ∷ Gen (Either Int (Either String (Either Bool ())))
gen_nested_either = Gen.either Gen.enumBounded do
  Gen.either (Gen.list (Range.linear 0 100) Gen.alphaNum) do
    Gen.either Gen.bool do pure ()

to_either ∷ OneOf '[x, y, z, w] → Either x (Either y (Either z w))
to_either = fmap (fmap (fmap unwrap . catch) . catch) . catch

from_either ∷ Either x (Either y (Either z w)) → OneOf '[x, y, z, w]
from_either = either inj (either inj (either inj inj))

hprop_OneOf_roundtrip ∷ Property
hprop_OneOf_roundtrip = property do
  x ← forAll gen_nested_either
  tripping x from_either (Just . to_either)
