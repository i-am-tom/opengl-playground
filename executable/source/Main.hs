{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (unless)
import Rendering.Display (withDisplay)
import Model.Loader ((~>))
import Model.Loader qualified as Model (create, destroy)
import Rendering.Controller (prepare, render)
import Rendering.Program qualified as Coloured
import Rendering.Program.Coloured (Coloured)
import Rendering.Program.Coloured qualified as Coloured (Model (..), create)
import SDL qualified
import SDL (V2 (V2), V3 (V3))
import Texture.Loader qualified as Texture

main :: IO ()
main = withDisplay SDL.defaultWindow \window -> do
  rawModel <- Model.create [ V3 0 1 3, V3 3 1 2 ]
    [ 0 ~>
        [ V3 (-0.5) ( 0.5) 0
        , V3 (-0.5) (-0.5) 0
        , V3 ( 0.5) (-0.5) 0
        , V3 ( 0.5) ( 0.5) 0
        ]

    , 1 ~>
        [ V3 0 0 1
        , V3 0 1 0
        , V3 0 1 1
        , V3 1 0 0
        ]
    ]

  let model :: Coloured.Model Coloured
      model = Coloured.Model { Coloured.rawModel = rawModel }

  shader <- Coloured.create

  untilWeQuit \events -> do
    prepare

    render shader model
    SDL.glSwapWindow window

  Model.destroy rawModel
  Coloured.destroy shader

-- main :: IO ()
-- main = withDisplay SDL.defaultWindow \window -> do
--   rawModel <- Model.create [ V3 0 1 3, V3 3 1 2 ]
--     [ 0 ~>
--         [ V3 (-0.5) ( 0.5) 0
--         , V3 (-0.5) (-0.5) 0
--         , V3 ( 0.5) (-0.5) 0
--         , V3 ( 0.5) ( 0.5) 0
--         ]
-- 
--     , 1 ~>
--         [ V2 0    0
--         , V2 0    0.25
--         , V2 0.25 0.25
--         , V2 0.25 0
--         ]
--     ]
-- 
--   texture <- Texture.fromTextureFile "resources/textures/beef.png"
-- 
--   let model :: Coloured.Model Coloured
--       model = Coloured.Model
--         { Coloured.rawModel = rawModel
--         , Coloured.texture  = texture
--         }
-- 
--   shader <- Coloured.create
-- 
--   untilWeQuit \events -> do
--     prepare
-- 
--     render shader model
--     SDL.glSwapWindow window
-- 
--   Model.destroy rawModel
--   Coloured.destroy shader

untilWeQuit :: ([SDL.Event] -> IO ()) -> IO ()
untilWeQuit action = do
  let isQuitEvent :: SDL.Event -> Bool
      isQuitEvent SDL.Event{ SDL.eventPayload }
        = eventPayload == SDL.QuitEvent

  SDL.pollEvents >>= \events ->
    unless (any isQuitEvent events) do
      action events *> untilWeQuit action
