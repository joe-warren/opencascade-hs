{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.Internal.Destructors where

import OpenCascade.BRepBuilderAPI.Types

import Foreign.Ptr

foreign import capi unsafe "hs_BRepBuilderAPI_MakeWire.h hs_delete_BRepBuilderAPI_MakeWire" deleteMakeWire :: Ptr MakeWire -> IO ()
foreign import capi unsafe "hs_BRepBuilderAPI_MakeFace.h hs_delete_BRepBuilderAPI_MakeFace" deleteMakeFace :: Ptr MakeFace -> IO ()
foreign import capi unsafe "hs_BRepBuilderAPI_MakeSolid.h hs_delete_BRepBuilderAPI_MakeSolid" deleteMakeSolid :: Ptr MakeSolid -> IO ()


