{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rendering.Display
  ( withDisplay
  ) where

import Control.Exception (bracket)
import SDL qualified
import System.IO (stderr)
import Text.Printf (hPrintf)

withDisplay :: SDL.WindowConfig -> (SDL.Window -> IO x) -> IO x
withDisplay config action
  = withWindow config \window ->
      withContext window (action window)

withWindow :: SDL.WindowConfig -> (SDL.Window -> IO x) -> IO x
withWindow windowConfig = bracket setup teardown
  where
    setup :: IO SDL.Window
    setup = do
      SDL.initialize [SDL.InitVideo]

      SDL.HintFramebufferAcceleration SDL.$= SDL.Enable3DOpenGL
      SDL.HintRenderDriver            SDL.$= SDL.OpenGL
      SDL.HintRenderOpenGLShaders     SDL.$= SDL.EnableShaders
      SDL.HintRenderScaleQuality      SDL.$= SDL.ScaleLinear

      hPrintf stderr "Creating window...\n"

      window <- SDL.createWindow "Display" windowConfig
        { SDL.windowGraphicsContext = SDL.OpenGLContext
            SDL.defaultOpenGL
              { SDL.glProfile = SDL.Core SDL.Debug 4 1
              }
        }

      hPrintf stderr "%s successfully created.\n" (show window)
      pure window

    teardown :: SDL.Window -> IO ()
    teardown window = SDL.destroyWindow window *> SDL.quit

withContext :: SDL.Window -> IO x -> IO x
withContext window action = bracket setup SDL.glDeleteContext \_ -> action
  where
    setup :: IO SDL.GLContext
    setup = do
      hPrintf stderr "Creating context...\n"
      context <- SDL.glCreateContext window

      hPrintf stderr "Attaching context to %s...\n" (show window)
      SDL.glMakeCurrent window context

      hPrintf stderr "Context successfully created and attached."
      pure context
