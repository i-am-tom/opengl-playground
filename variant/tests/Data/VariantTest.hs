{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeApplications #-}

module Data.VariantTest where

import Data.Variant
import Test.Hspec (Spec, it, shouldBe)
import Text.Read (readMaybe)

spec_Variant ∷ Spec
spec_Variant = do
  it "Value injection" do
    let example ∷ xs ~ '[ String, Bool, () ] ⇒ Inject xs x => x → Variant xs
        example = inject

    example "hello" `shouldBe` Choice Here "hello"
    example True `shouldBe` Choice (There Here) True
    example () `shouldBe` Choice (There (There Here)) ()

  it "Partial value injection" do
    let example ∷ xs ~ '[ String, Bool, () ] ⇒ Offer xs x ⇒ x → Maybe (Variant xs)
        example = offer

    example "hello" `shouldBe` Just (Choice Here "hello")
    example True `shouldBe` Just (Choice (There Here) True)
    example () `shouldBe` Just (Choice (There (There Here)) ())
    example (3 ∷ Int) `shouldBe` Nothing

  it "Interpretation" do
    let example ∷ String → Maybe (Variant '[Int, Bool])
        example = interpret @Read @'[Int, Bool] readMaybe

    example "3" `shouldBe` Just (Choice Here 3)
    example "True" `shouldBe` Just (Choice (There Here) True)
    example "()" `shouldBe` Nothing

  it "Folding" do
    let example ∷ Variant '[Int, Bool, String] → String
        example x = fold @Show x show

    example (Choice Here 3) `shouldBe` "3"
    example (Choice (There Here) True) `shouldBe` "True"
    example (Choice (There (There Here)) "hello") `shouldBe` "\"hello\""

  it "Translating" do
    let example ∷ Variant '[Int, Bool] → Maybe (Variant '[Bool, String])
        example = translate

    example (Choice Here 3) `shouldBe` Nothing
    example (Choice (There Here) True) `shouldBe` Just (Choice Here True)
