{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UnicodeSyntax #-}

module Wavefront.AggregateTest where

import Linear (V2 (V2), V3 (V3))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec.ByteString (parseFromFile)
import Wavefront.Aggregate
import Wavefront.Parser (parser)

spec_examples ∷ Spec
spec_examples = describe "Spec examples" do
  it "square facing towards the camera" do
    parseFromFile parser "examples/square.obj" >>= mapM_ \result →
      aggregate result `shouldBe` Aggregate
        { buckets = Buckets
            { vertices =
                [ V3 0 0 0
                , V3 0 2 0
                , V3 2 0 0
                , V3 2 2 0
                ]

            , textures = replicate 4 $ V2 0 0
            , normals  = replicate 4 $ V3 1 0 1
            }

        , faces =
            [ V3 1 0 2
            , V3 2 3 1
            ]
        }

  it "cube with each vertex shared by three faces" do
    parseFromFile parser "examples/cube.obj" >>= mapM_  \result →
      aggregate result `shouldBe` Aggregate
        { buckets = Buckets
            { vertices =
                [ V3 (-0.5) (-0.5) (-0.5)
                , V3 (-0.5) (-0.5)   0.5
                , V3 (-0.5)   0.5  (-0.5)
                , V3 (-0.5)   0.5    0.5
                , V3   0.5  (-0.5) (-0.5)
                , V3   0.5  (-0.5)   0.5
                , V3   0.5    0.5  (-0.5)
                , V3   0.5    0.5    0.5
                ]
            
            , textures = replicate 8 $ V2 0 0
            , normals  = replicate 8 $ V3 1 0 1
            }
            
        , faces =
            [ V3 2 0 6
            , V3 6 0 4
            , V3 3 1 7
            , V3 7 1 5
            , V3 6 4 7
            , V3 7 4 5
            , V3 2 0 3
            , V3 3 0 1
            , V3 3 2 7
            , V3 7 2 6
            , V3 1 0 5
            , V3 5 0 4
            ]
        }
