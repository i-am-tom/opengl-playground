{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Functions for receiving mouse input.
module Interface.Mouse where

import Control.Monad.IO.Class (MonadIO)
import FRP.Behaviour
import FRP.Event
import Foreign.C.Types (CInt)
import Linear (V2)
import SDL qualified

-- | When in 'SDL.RelativeLocation', we talk about the mouse as a cumulative
-- movement of pixels away from the origin. This behaviour can be sampled to
-- get the current total @x@ and @y@ offset from the origin.
relativeMouseLocation ∷ MonadIO m ⇒ Behaviour m (V2 CInt)
relativeMouseLocation = Behaviour \event →
  Event \k → do
    subscribe event \value → do
      position ← SDL.getRelativeMouseLocation
      k (value position)
