{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (unless)
import Rendering.Display (withDisplay)
import Model.Loader (create)
import Rendering.Controller (prepare, render)
import SDL qualified
import SDL (V3 (V3))

main :: IO ()
main = withDisplay SDL.defaultWindow \window -> do
  model <- create
    [ V3 (-0.5) ( 0.5) 0
    , V3 (-0.5) (-0.5) 0
    , V3 ( 0.5) (-0.5) 0
    , V3 ( 0.5) (-0.5) 0
    , V3 ( 0.5) ( 0.5) 0
    , V3 (-0.5) ( 0.5) 0
    ]

  untilWeQuit \events -> do
    prepare *> render model

untilWeQuit :: ([SDL.Event] -> IO ()) -> IO ()
untilWeQuit action = do
  let isQuitEvent :: SDL.Event -> Bool
      isQuitEvent SDL.Event{ SDL.eventPayload } = eventPayload == SDL.QuitEvent

  SDL.pollEvents >>= \events ->
    unless (any isQuitEvent events) do
      action events *> untilWeQuit action
