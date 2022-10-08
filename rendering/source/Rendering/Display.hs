{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rendering.Display where

import Control.Exception (bracket)
import SDL qualified

withDisplay :: SDL.WindowConfig -> (SDL.Window -> IO x) -> IO x
withDisplay windowConfig action
  = withWindow windowConfig \window ->
      withContext window (action window)

withWindow :: SDL.WindowConfig -> (SDL.Window -> IO x) -> IO x
withWindow windowConfig = bracket setup teardown
  where
    setup :: IO SDL.Window
    setup = do
      SDL.initializeAll

      SDL.HintFramebufferAcceleration SDL.$= SDL.Enable3DOpenGL
      SDL.HintRenderDriver            SDL.$= SDL.OpenGL
      SDL.HintRenderOpenGLShaders     SDL.$= SDL.EnableShaders
      SDL.HintRenderScaleQuality      SDL.$= SDL.ScaleLinear

      SDL.createWindow "Display" windowConfig
        { SDL.windowGraphicsContext = SDL.OpenGLContext
            SDL.defaultOpenGL
              { SDL.glProfile = SDL.Core SDL.Debug 4 3
              }
        }

    teardown :: SDL.Window -> IO ()
    teardown window = do
      SDL.destroyWindow window
      SDL.quit

withContext :: SDL.Window -> IO x -> IO x
withContext window action = bracket setup teardown \_ -> action
  where
    setup :: IO SDL.GLContext
    setup = SDL.glCreateContext window

    teardown :: SDL.GLContext -> IO ()
    teardown = SDL.glDeleteContext
