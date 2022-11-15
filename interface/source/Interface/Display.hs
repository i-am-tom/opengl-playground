{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Functions for producing a display in the interface.
module Interface.Display where

import Control.Exception (bracket)
import SDL qualified

-- | Create a new SDL window with the given config, cleaning up once the given
-- continuation has been completed.
with ∷ ∀ x. SDL.WindowConfig → (SDL.Window → IO x) → IO x
with windowConfig k = bracket setup teardown \(_, window) → k window
  where
    setup ∷ IO (SDL.GLContext, SDL.Window)
    setup = do
      SDL.initializeAll

      SDL.HintFramebufferAcceleration SDL.$= SDL.Enable3DOpenGL
      SDL.HintRenderDriver            SDL.$= SDL.OpenGL
      SDL.HintRenderOpenGLShaders     SDL.$= SDL.EnableShaders
      SDL.HintRenderScaleQuality      SDL.$= SDL.ScaleLinear

      window <- SDL.createWindow "Display" windowConfig
        { SDL.windowGraphicsContext = SDL.OpenGLContext
            SDL.defaultOpenGL
              { SDL.glProfile = SDL.Core SDL.Debug 4 1
              }
        }

      context <- SDL.glCreateContext window

      SDL.glMakeCurrent window context
      pure (context, window)

    teardown :: (SDL.GLContext, SDL.Window) -> IO ()
    teardown (context, window) = do
      SDL.glDeleteContext context
      SDL.destroyWindow window
      SDL.quit
