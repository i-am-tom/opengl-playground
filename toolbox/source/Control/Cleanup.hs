{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Control.Cleanup where

import Data.IORef (modifyIORef, newIORef)
import Data.Kind (Type)
import Graphics.Rendering.OpenGL qualified as GL

type Cleanup :: Type
newtype Cleanup = Cleanup { run :: IO () }

withCleanup :: ((Cleanup -> IO ()) -> IO x) -> IO x
withCleanup action = do
  queue <- newIORef mempty

  action \cleanup ->
    modifyIORef queue \cleanups ->
      cleanups *> run cleanup

cleanup :: IO () -> Cleanup
cleanup = Cleanup

cleanupNamedObject :: GL.ObjectName x => x -> Cleanup
cleanupNamedObject = cleanup . GL.deleteObjectName
