{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Control.Cleanup where

import Data.IORef (modifyIORef, newIORef)
import Data.Kind (Type)

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
