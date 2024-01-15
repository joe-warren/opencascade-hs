{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepMesh.Internal.Destructors 
( deleteIncrementalMesh
) where

import OpenCascade.BRepMesh.Types

import Foreign.Ptr

foreign import capi unsafe "hs_BRepMesh_IncrementalMesh.h hs_delete_BRepMesh_IncrementalMesh" deleteIncrementalMesh :: Ptr IncrementalMesh -> IO ()

