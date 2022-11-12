{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeApplications #-}

module Data.VariantTest where

import Data.Function ((&))
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

  it "Value projection" do
    let x ∷ (Inject xs String, Inject xs Int, Inject xs Bool) ⇒ Variant xs
        x = inject "hello"

        y ∷ (Inject xs String, Inject xs Int, Inject xs Bool) ⇒ Variant xs
        y = inject (3 ∷ Int)

        z ∷ (Inject xs String, Inject xs Int, Inject xs Bool) ⇒ Variant xs
        z = inject True

    catch x `shouldBe` Right @(Variant '[Int, Bool]) "hello"
    catch y `shouldBe` Right @(Variant '[String, Bool]) (3 ∷ Int)
    catch z `shouldBe` Right @(Variant '[String, Int]) True

    -- Mismatch
    catch x `shouldBe` Left @(Variant '[String, Bool]) @Int (Choice Here "hello")

  it "Case matching" do
    let x ∷ Variant '[String, Int, Bool]
        x = inject "hello"

        y ∷ Variant '[String, Int, Bool]
        y = inject (3 ∷ Int)

        z ∷ Variant '[String, Int, Bool]
        z = inject True

        strip ∷ Variant '[String, Int, Bool] → String
        strip e = given e
          & match (\s → "STRING: " ++ s)
          & match (\i → "INT: " ++ show i)
          & match (\b → "BOOL: " ++ show b)
          & qed

    strip x `shouldBe` "STRING: hello"
    strip y `shouldBe` "INT: 3"
    strip z `shouldBe` "BOOL: True"

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
