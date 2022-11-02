{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module Barbies.Extended where

import Barbies (AllB, ApplicativeB, ConstraintsB, TraversableB)
import Barbies qualified as B
import Data.Function ((&))
import Data.Functor.Product (Product (Pair))
import Data.Monoid (Ap (Ap, getAp))

bzipWithM_
  ∷ ∀ c b m f g
  . ( AllB c b
    , ApplicativeB b
    , ConstraintsB b
    , TraversableB b
    , Applicative m
    )
  ⇒ (∀ x. c x ⇒ f x → g x → m ()) → b f → b g → m ()
bzipWithM_ f xs ys = bfoldMapCA @c go (B.bprod xs ys) 
  where go (Pair x y) = f x y

bfoldMapCA
  ∷ ∀ c b m f
  . ( AllB c b
    , ConstraintsB b
    , TraversableB b
    , Applicative m
    )
  ⇒ (∀ x. c x ⇒ f x → m ()) → b f → m ()
bfoldMapCA f = getAp . B.bfoldMapC @c (Ap . f)
