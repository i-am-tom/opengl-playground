{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (unless)
import Rendering.Display (withWindow)
import SDL qualified

main :: IO ()
main = withWindow SDL.defaultWindow \window -> do
  untilWeQuit \events -> do
    pure ()

untilWeQuit :: ([SDL.Event] -> IO ()) -> IO ()
untilWeQuit action = do
  let isQuitEvent :: SDL.Event -> Bool
      isQuitEvent SDL.Event{ SDL.eventPayload } = eventPayload == SDL.QuitEvent

  SDL.pollEvents >>= \events ->
    unless (any isQuitEvent events) do
      action events *> untilWeQuit action
