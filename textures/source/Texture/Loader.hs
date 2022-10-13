{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

module Texture.Loader where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Graphics.GLUtil qualified as GL (readTexture, texture2DWrap)
import Graphics.Rendering.OpenGL qualified as GL
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (hPrintf)

fromTextureFile ∷ FilePath → IO GL.TextureObject
fromTextureFile filepath = do
  GL.readTexture filepath >>= \case
    Right object → do
      GL.textureFilter GL.Texture2D GL.$=! ((GL.Linear', Nothing), GL.Linear')
      GL.texture2DWrap GL.$=! (GL.Repeated, GL.Repeat)

      pure object

    Left message → do
      hPrintf stderr "Error while loading texture '%s'\n" filepath
      hPutStrLn stderr message *> exitFailure
