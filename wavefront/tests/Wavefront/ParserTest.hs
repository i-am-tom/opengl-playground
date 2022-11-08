{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Wavefront.ParserTest where

import Data.Kind (Type)
import Data.OneOf (OneOf, inj)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec.ByteString (parseFromFile)
import Wavefront.Parser hiding (comment, i, j, k, v, x, y, z)

spec_examples ∷ Spec
spec_examples = describe "Spec examples" do
  it "square facing towards the camera" do
    result ← parseFromFile parser "examples/square.obj"

    result `shouldBe` Right
      [ v 0.000000 2.000000 0.000000
      , v 0.000000 0.000000 0.000000
      , v 2.000000 0.000000 0.000000
      , v 2.000000 2.000000 0.000000

      , fv [ 1, 2, 3, 4 ]
      ]

  it "cube with each vertex shared by three faces" do
    result ← parseFromFile parser "examples/cube.obj"

    result `shouldBe` Right
      [ v 0.000000 2.000000 2.000000
      , v 0.000000 0.000000 2.000000
      , v 2.000000 0.000000 2.000000
      , v 2.000000 2.000000 2.000000
      , v 0.000000 2.000000 0.000000
      , v 0.000000 0.000000 0.000000
      , v 2.000000 0.000000 0.000000
      , v 2.000000 2.000000 0.000000

      , fv [ 1, 2, 3, 4 ]
      , fv [ 8, 7, 6, 5 ]
      , fv [ 4, 3, 7, 8 ]
      , fv [ 5, 1, 4, 8 ]
      , fv [ 5, 6, 2, 1 ]
      , fv [ 2, 6, 7, 3 ]
      ]

  it "cube with negative vertex reference numbers" do
    result ← parseFromFile parser "examples/cube-with-negative-reference-numbers.obj"

    result `shouldBe` Right
      [ v 0.000000 2.000000 2.000000
      , v 0.000000 0.000000 2.000000
      , v 2.000000 0.000000 2.000000
      , v 2.000000 2.000000 2.000000
      , fv [ (-4), (-3), (-2), (-1) ]

      , v 2.000000 2.000000 0.000000
      , v 2.000000 0.000000 0.000000
      , v 0.000000 0.000000 0.000000
      , v 0.000000 2.000000 0.000000
      , fv [ (-4), (-3), (-2), (-1) ]

      , v 2.000000 2.000000 2.000000
      , v 2.000000 0.000000 2.000000
      , v 2.000000 0.000000 0.000000
      , v 2.000000 2.000000 0.000000
      , fv [ (-4), (-3), (-2), (-1) ]

      , v 0.000000 2.000000 0.000000
      , v 0.000000 2.000000 2.000000
      , v 2.000000 2.000000 2.000000
      , v 2.000000 2.000000 0.000000
      , fv [ (-4), (-3), (-2), (-1) ]

      , v 0.000000 2.000000 0.000000
      , v 0.000000 0.000000 0.000000
      , v 0.000000 0.000000 2.000000
      , v 0.000000 2.000000 2.000000
      , fv [ (-4), (-3), (-2), (-1) ]

      , v 0.000000 0.000000 2.000000
      , v 0.000000 0.000000 0.000000
      , v 2.000000 0.000000 0.000000
      , v 2.000000 0.000000 2.000000
      , fv [ (-4), (-3), (-2), (-1) ]
      ]

  it "cube with each of its faces placed in a separate group" do
    result ← parseFromFile parser "examples/cube-with-group-names.obj"

    result `shouldBe` Right
      [ v 0.000000 2.000000 2.000000
      , v 0.000000 0.000000 2.000000
      , v 2.000000 0.000000 2.000000
      , v 2.000000 2.000000 2.000000
      , v 0.000000 2.000000 0.000000
      , v 0.000000 0.000000 0.000000
      , v 2.000000 0.000000 0.000000
      , v 2.000000 2.000000 0.000000
      , comment "8 vertices"

      , g [ "front", "cube" ]
      , fv [ 1, 2, 3, 4 ]

      , g [ "back", "cube" ]
      , fv [ 8, 7, 6, 5 ]

      , g [ "right", "cube" ]
      , fv [ 4, 3, 7, 8 ]

      , g [ "top", "cube" ]
      , fv [ 5, 1, 4, 8 ]

      , g [ "left", "cube" ]
      , fv [ 5, 6, 2, 1 ]

      , g [ "bottom", "cube" ]
      , fv [ 2, 6, 7, 3 ]

      , comment "6 elements"
      ]

  it "two adjoining squares that share a common edge" do
    result ← parseFromFile parser "examples/two-adjoining-squares-with-a-smoothing-group.obj"

    result `shouldBe` Right
      [ v 0.000000 2.000000 0.000000
      , v 0.000000 0.000000 0.000000
      , v 2.000000 0.000000 0.000000
      , v 2.000000 2.000000 0.000000
      , v 4.000000 0.000000 (-1.255298)
      , v 4.000000 2.000000 (-1.255298)
      , comment "6 vertices"

      , g [ "all" ]
      , s (Just 1)

      , fv [ 1, 2, 3, 4 ]
      , fv [ 4, 3, 5, 6 ]

      , comment "2 elements"
      ]

  it "two squares that share a common edge (with vertex normals)" do
    result ← parseFromFile parser "examples/two-adjoining-squares-with-vertex-normals.obj"

    result `shouldBe` Right
      [ v 0.000000 2.000000 0.000000
      , v 0.000000 0.000000 0.000000
      , v 2.000000 0.000000 0.000000
      , v 2.000000 2.000000 0.000000
      , v 4.000000 0.000000 (-1.255298)
      , v 4.000000 2.000000 (-1.255298)
      , vn 0.000000 0.000000 1.000000
      , vn 0.000000 0.000000 1.000000
      , vn 0.276597 0.000000 0.960986
      , vn 0.276597 0.000000 0.960986
      , vn 0.531611 0.000000 0.846988
      , vn 0.531611 0.000000 0.846988

      , comment "6 vertices"
      , comment "6 normals"
    
      , g [ "all" ]
      , s (Just 1)
      , fvn [ (1, 1), (2, 2), (3, 3), (4, 4) ]
      , fvn [ (4, 4), (3, 3), (5, 5), (6, 6) ]
      , comment "2 elements"
      ]

-- * A little DSL for making copy/pasting examples easier.

type Commands ∷ [Type]
type Commands =
  '[ GeometricVertex
   , VertexNormal
   , TextureVertex
   , Points
   , Line
   , Face
   , GroupNames
   , SmoothingGroup
   , ObjectName
   , Comment
   ]

comment ∷ String → OneOf Commands
comment = inj . Comment

v ∷ Double → Double → Double → OneOf Commands
v x y z = inj $ GeometricVertex x y z

vn ∷ Double → Double → Double → OneOf Commands
vn i j k = inj $ VertexNormal i j k

fv ∷ [Int] → OneOf Commands
fv vs = f [( vertex, Nothing, Nothing ) | vertex ← vs]

fvn ∷ [(Int, Int)] → OneOf Commands
fvn vs = f [( vertex, Nothing, Just normal ) | (vertex, normal) ← vs]

f ∷ [(Int, Maybe Int, Maybe Int)] → OneOf Commands
f xs = inj $ Face
  [ (Index vertex, fmap Index texture, fmap Index normal)
  | (vertex, texture, normal) ← xs
  ]

g ∷ [String] → OneOf Commands
g = inj . GroupNames

s ∷ Maybe Int → OneOf Commands
s = inj . SmoothingGroup
