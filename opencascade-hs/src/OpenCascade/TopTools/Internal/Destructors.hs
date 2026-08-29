{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopTools.Internal.Destructors
( deleteListOfShape
, deleteIndexedDataMapOfShapeListOfShape
) where

import OpenCascade.TopTools.Types
import Foreign.Ptr

foreign import capi unsafe "hs_TopTools_ListOfShape.h hs_delete_TopTools_ListOfShape" deleteListOfShape :: Ptr ListOfShape -> IO ()

foreign import capi unsafe "hs_TopTools_IndexedDataMapOfShapeListOfShape.h hs_delete_TopTools_IndexedDataMapOfShapeListOfShape" deleteIndexedDataMapOfShapeListOfShape :: Ptr IndexedDataMapOfShapeListOfShape -> IO ()
