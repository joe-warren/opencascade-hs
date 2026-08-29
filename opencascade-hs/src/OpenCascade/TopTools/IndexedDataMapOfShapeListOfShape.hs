{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopTools.IndexedDataMapOfShapeListOfShape
( IndexedDataMapOfShapeListOfShape
, extent
, contains
, findFromKey
, findKey
, findFromIndex
) where

import OpenCascade.TopTools.Types (ListOfShape, IndexedDataMapOfShapeListOfShape)
import OpenCascade.TopTools.Internal.Destructors (deleteListOfShape)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CInt (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_TopTools_IndexedDataMapOfShapeListOfShape.h hs_TopTools_IndexedDataMapOfShapeListOfShape_extent" rawExtent :: Ptr IndexedDataMapOfShapeListOfShape -> IO CInt

extent :: Ptr IndexedDataMapOfShapeListOfShape -> IO Int
extent = fmap fromIntegral . rawExtent

foreign import capi unsafe "hs_TopTools_IndexedDataMapOfShapeListOfShape.h hs_TopTools_IndexedDataMapOfShapeListOfShape_contains" rawContains :: Ptr IndexedDataMapOfShapeListOfShape -> Ptr TopoDS.Shape -> IO CBool

contains :: Ptr IndexedDataMapOfShapeListOfShape -> Ptr TopoDS.Shape -> IO Bool
contains theMap shape = cBoolToBool <$> rawContains theMap shape

foreign import capi unsafe "hs_TopTools_IndexedDataMapOfShapeListOfShape.h hs_TopTools_IndexedDataMapOfShapeListOfShape_findFromKey" rawFindFromKey
    :: Ptr IndexedDataMapOfShapeListOfShape
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr ListOfShape)

findFromKey :: Ptr IndexedDataMapOfShapeListOfShape -> Ptr TopoDS.Shape -> Acquire (Ptr ListOfShape)
findFromKey theMap shape = mkAcquire (wrapException $ rawFindFromKey theMap shape) deleteListOfShape

foreign import capi unsafe "hs_TopTools_IndexedDataMapOfShapeListOfShape.h hs_TopTools_IndexedDataMapOfShapeListOfShape_findKey" rawFindKey
    :: Ptr IndexedDataMapOfShapeListOfShape
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

-- index is 1 based
findKey :: Ptr IndexedDataMapOfShapeListOfShape -> Int -> Acquire (Ptr TopoDS.Shape)
findKey theMap index = mkAcquire (wrapException $ rawFindKey theMap (fromIntegral index)) deleteShape

foreign import capi unsafe "hs_TopTools_IndexedDataMapOfShapeListOfShape.h hs_TopTools_IndexedDataMapOfShapeListOfShape_findFromIndex" rawFindFromIndex
    :: Ptr IndexedDataMapOfShapeListOfShape
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr ListOfShape)

-- index is 1 based
findFromIndex :: Ptr IndexedDataMapOfShapeListOfShape -> Int -> Acquire (Ptr ListOfShape)
findFromIndex theMap index = mkAcquire (wrapException $ rawFindFromIndex theMap (fromIntegral index)) deleteListOfShape
