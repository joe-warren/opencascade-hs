{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopoDS.Internal.Destructors 
( deleteShape
, deleteBuilder
) where

import OpenCascade.TopoDS.Types

import Foreign.Ptr

foreign import capi unsafe "hs_TopoDS_Shape.h hs_delete_TopoDS_Shape" deleteShape :: Ptr Shape -> IO ()

foreign import capi unsafe "hs_TopoDS_Builder.h hs_delete_TopoDS_Builder" deleteBuilder :: Ptr Builder -> IO ()

