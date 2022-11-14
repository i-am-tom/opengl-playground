{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Wavefront.ParserTest where

import Linear (V2 (V2), V3 (V3))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec.ByteString (parseFromFile)
import Wavefront.Parser (Command (..), parser)

spec_examples ∷ Spec
spec_examples = describe "Spec examples" do
  it "square facing towards the camera" do
    result ← parseFromFile parser "examples/square.obj"

    result `shouldBe` Right
      [ GeometricVertex (V3 0.000000 2.000000 0.000000)
      , GeometricVertex (V3 0.000000 0.000000 0.000000)
      , GeometricVertex (V3 2.000000 0.000000 0.000000)
      , GeometricVertex (V3 2.000000 2.000000 0.000000)
      , TextureVertex   (V2 0.000000 0.000000         )
      , VertexNormal    (V3 1.000000 0.000000 1.000000)

      , Triangle $ V3 (V3 1 1 1) (V3 2 1 1) (V3 3 1 1)
      , Triangle $ V3 (V3 3 1 1) (V3 4 1 1) (V3 1 1 1)
      ]

  it "cube with each vertex shared by three faces" do
    result ← parseFromFile parser "examples/cube.obj"

    result `shouldBe` Right
      [ GeometricVertex (V3 (-0.5) ( 0.5) (-0.5))
      , GeometricVertex (V3 (-0.5) (-0.5) (-0.5))
      , GeometricVertex (V3 ( 0.5) (-0.5) (-0.5))
      , GeometricVertex (V3 ( 0.5) ( 0.5) (-0.5))
      , GeometricVertex (V3 (-0.5) ( 0.5) ( 0.5))
      , GeometricVertex (V3 (-0.5) (-0.5) ( 0.5))
      , GeometricVertex (V3 ( 0.5) (-0.5) ( 0.5))
      , GeometricVertex (V3 ( 0.5) ( 0.5) ( 0.5))
      , GeometricVertex (V3 ( 0.5) ( 0.5) (-0.5))
      , GeometricVertex (V3 ( 0.5) (-0.5) (-0.5))
      , GeometricVertex (V3 ( 0.5) (-0.5) ( 0.5))
      , GeometricVertex (V3 ( 0.5) ( 0.5) ( 0.5))
      , GeometricVertex (V3 (-0.5) ( 0.5) (-0.5))
      , GeometricVertex (V3 (-0.5) (-0.5) (-0.5))
      , GeometricVertex (V3 (-0.5) (-0.5) ( 0.5))
      , GeometricVertex (V3 (-0.5) ( 0.5) ( 0.5))
      , GeometricVertex (V3 (-0.5) ( 0.5) ( 0.5))
      , GeometricVertex (V3 (-0.5) ( 0.5) (-0.5))
      , GeometricVertex (V3 ( 0.5) ( 0.5) (-0.5))
      , GeometricVertex (V3 ( 0.5) ( 0.5) ( 0.5))
      , GeometricVertex (V3 (-0.5) (-0.5) ( 0.5))
      , GeometricVertex (V3 (-0.5) (-0.5) (-0.5))
      , GeometricVertex (V3 ( 0.5) (-0.5) (-0.5))
      , GeometricVertex (V3 ( 0.5) (-0.5) ( 0.5))

      , TextureVertex $ V2 0.000000 0.000000
      , VertexNormal  $ V3 1.000000 0.000000 1.000000

      , Triangle $ V3 (V3 1  1 1) (V3 2  1 1) (V3 4  1 1)
      , Triangle $ V3 (V3 4  1 1) (V3 2  1 1) (V3 3  1 1)
      , Triangle $ V3 (V3 5  1 1) (V3 6  1 1) (V3 8  1 1)
      , Triangle $ V3 (V3 8  1 1) (V3 6  1 1) (V3 7  1 1)
      , Triangle $ V3 (V3 9  1 1) (V3 10 1 1) (V3 12 1 1)
      , Triangle $ V3 (V3 12 1 1) (V3 10 1 1) (V3 11 1 1)
      , Triangle $ V3 (V3 13 1 1) (V3 14 1 1) (V3 16 1 1)
      , Triangle $ V3 (V3 16 1 1) (V3 14 1 1) (V3 15 1 1)
      , Triangle $ V3 (V3 17 1 1) (V3 18 1 1) (V3 20 1 1)
      , Triangle $ V3 (V3 20 1 1) (V3 18 1 1) (V3 19 1 1)
      , Triangle $ V3 (V3 21 1 1) (V3 22 1 1) (V3 24 1 1)
      , Triangle $ V3 (V3 24 1 1) (V3 22 1 1) (V3 23 1 1)
      ]
