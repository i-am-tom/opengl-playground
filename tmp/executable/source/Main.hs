{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function ((&))
import Data.IORef (modifyIORef, newIORef, readIORef)
import Model.Loader ((~>))
import Model.Loader qualified as Model (create, destroy)
import Model.Textured qualified as Textured
import Rendering.Component.Camera (Camera (..), camera)
import Rendering.Component.Project (project)
import Rendering.Component.Transform (Transform (..), transform)
import Rendering.Controller (prepare, render)
import Rendering.Delta qualified as Delta
import Rendering.Display (withDisplay)
import Rendering.Program qualified as Program
import Rendering.Program.Coloured (Coloured)
import Rendering.Program.Coloured qualified as Coloured (create)
import Rendering.Program.Textured (Textured)
import Rendering.Program.Textured qualified as Textured (create)
import SDL (V2 (V2), V3 (V3), axisAngle)
import SDL qualified
import Texture.Loader qualified as Texture

main ∷ IO ()
main = coloured

coloured ∷ IO ()
coloured = withDisplay SDL.defaultWindow \window → do
  model ← Model.create [ V3 0 1 3, V3 3 1 2 ]
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

  shader ← Coloured.create
  yaw    ← liftIO (newIORef 0)

  untilWeQuit \events → do
    prepare

    r ← modifyIORef yaw (+ 0.01) *> readIORef yaw

    let eg0 ∷ Transform
        eg0 = Transform
          { translate = V3 (-0.5) 0 (-2)
          , rotate = 0
          , scale = 1
          }

    let eg1 ∷ Transform
        eg1 = Transform
          { translate = V3 0.5 0 (-3)
          , rotate = 0
          , scale = 1
          }

    let view ∷ Camera
        view = Camera
          { position = V3 0 0 10
          , pitch = r
          , yaw = 0
          }

    render shader model
      [ Delta.bunchanged
          & transform (Delta.Changed eg0)
          & camera (Delta.Changed view)
          & project (Delta.Changed (SDL.perspective 0.8 1 0.1 1000))

      , Delta.bunchanged
          & transform (Delta.Changed eg1)
      ]

    SDL.glSwapWindow window

  Model.destroy model
  Program.destroy shader

textured ∷ IO ()
textured = withDisplay SDL.defaultWindow \window → do
  rawModel ← Model.create [ V3 0 1 3, V3 3 1 2 ]
    [ 0 ~>
        [ V3 (-0.5) ( 0.5) 0
        , V3 (-0.5) (-0.5) 0
        , V3 ( 0.5) (-0.5) 0
        , V3 ( 0.5) ( 0.5) 0
        ]

    , 1 ~>
        [ V2 0    0
        , V2 0    0.25
        , V2 0.25 0.25
        , V2 0.25 0
        ]
    ]

  texture ← Texture.fromTextureFile "resources/textures/beef.png"

  let model ∷ Program.Model Textured
      model = Textured.Model
        { Textured.model   = rawModel
        , Textured.texture = texture
        }

  shader ← Textured.create

  untilWeQuit \events → do
    prepare

    render shader model []
    SDL.glSwapWindow window

  Model.destroy rawModel
  Program.destroy shader

untilWeQuit ∷ ([SDL.Event] → IO ()) → IO ()
untilWeQuit action = do
  let isQuitEvent ∷ SDL.Event → Bool
      isQuitEvent SDL.Event{ SDL.eventPayload }
        = eventPayload == SDL.QuitEvent

  SDL.pollEvents >>= \events →
    unless (any isQuitEvent events) do
      action events *> untilWeQuit action
