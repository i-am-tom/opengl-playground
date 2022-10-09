{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (unless)
import Rendering.Display (withDisplay)
import Model.Loader qualified as Model (create, destroy)
import Rendering.Controller (prepare, render)
import Rendering.Program qualified as Shader (destroy, withRenderingProgram)
import Rendering.Program.Static qualified as Static (create)
import SDL qualified
import SDL (V3 (V3))

main :: IO ()
main = withDisplay SDL.defaultWindow \window -> do
  model <- Model.create [ V3 0 1 3, V3 3 1 2 ]
    [ V3 (-0.5) ( 0.5) 0
    , V3 (-0.5) (-0.5) 0
    , V3 ( 0.5) (-0.5) 0
    , V3 ( 0.5) ( 0.5) 0
    ]

  shader <- Static.create

  untilWeQuit \events -> do
    prepare

    Shader.withRenderingProgram shader do
      render model

    SDL.glSwapWindow window

  Model.destroy model
  Shader.destroy shader

untilWeQuit :: ([SDL.Event] -> IO ()) -> IO ()
untilWeQuit action = do
  let isQuitEvent :: SDL.Event -> Bool
      isQuitEvent SDL.Event{ SDL.eventPayload } = eventPayload == SDL.QuitEvent

  SDL.pollEvents >>= \events ->
    unless (any isQuitEvent events) do
      action events *> untilWeQuit action
