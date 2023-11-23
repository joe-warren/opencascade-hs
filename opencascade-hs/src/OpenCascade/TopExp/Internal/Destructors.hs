{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopExp.Internal.Destructors where

import OpenCascade.TopExp.Types

import Foreign.Ptr

foreign import capi unsafe "hs_TopExp_Explorer.h hs_delete_TopExp_Explorer" deleteExplorer :: Ptr Explorer -> IO ()


