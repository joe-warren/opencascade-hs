{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepOffsetAPI.Internal.Destructors where

import OpenCascade.BRepOffsetAPI.Types

import Foreign.Ptr

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipe.h hs_delete_BRepOffsetAPI_MakePipe" deleteMakePipe :: Ptr MakePipe -> IO ()


