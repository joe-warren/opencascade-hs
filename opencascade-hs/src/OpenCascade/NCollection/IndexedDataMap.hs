{-# LANGUAGE CApiFFI #-}
module OpenCascade.NCollection.IndexedDataMap
( IndexedDataMap
, newAsciiStringMap
, newShapeListOfShapeMap
, extentShapeListOfShapeMap
, containsShapeListOfShapeMap
, findFromKeyShapeListOfShapeMap
, findKeyShapeListOfShapeMap
, findFromIndexShapeListOfShapeMap
) where

import OpenCascade.NCollection.Types (IndexedDataMap, List)
import OpenCascade.NCollection.Internal.Destructors (deleteAsciiStringMap, deleteIndexedDataMapOfShapeListOfShape, deleteListOfShape)
import qualified OpenCascade.TCollection.Types as TCollection
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CInt (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_NCollection_IndexedDataMap.h hs_new_NCollection_IndexedDataMap_TCollection_AsciiString_TCollection_AsciiString" rawNewAsciiStringMap
    :: IO (Ptr (IndexedDataMap TCollection.AsciiString TCollection.AsciiString))

newAsciiStringMap :: Acquire (Ptr (IndexedDataMap TCollection.AsciiString TCollection.AsciiString))
newAsciiStringMap = mkAcquire rawNewAsciiStringMap deleteAsciiStringMap

foreign import capi unsafe "hs_NCollection_IndexedDataMap.h hs_new_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape" rawNewShapeListOfShapeMap
    :: IO (Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape)))

newShapeListOfShapeMap :: Acquire (Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape)))
newShapeListOfShapeMap = mkAcquire rawNewShapeListOfShapeMap deleteIndexedDataMapOfShapeListOfShape

foreign import capi unsafe "hs_NCollection_IndexedDataMap.h hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_extent" rawExtentShapeListOfShapeMap
    :: Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape)) -> IO CInt

extentShapeListOfShapeMap :: Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape)) -> IO Int
extentShapeListOfShapeMap = fmap fromIntegral . rawExtentShapeListOfShapeMap

foreign import capi unsafe "hs_NCollection_IndexedDataMap.h hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_contains" rawContainsShapeListOfShapeMap
    :: Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape)) -> Ptr TopoDS.Shape -> IO CBool

containsShapeListOfShapeMap :: Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape)) -> Ptr TopoDS.Shape -> IO Bool
containsShapeListOfShapeMap theMap shape = cBoolToBool <$> rawContainsShapeListOfShapeMap theMap shape

foreign import capi unsafe "hs_NCollection_IndexedDataMap.h hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_findFromKey" rawFindFromKeyShapeListOfShapeMap
    :: Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape))
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (List TopoDS.Shape))

-- | The list associated with a key.
--
-- Throws @NoSuchObject@ if the key is not in the map; check with 'containsShapeListOfShapeMap' first.
findFromKeyShapeListOfShapeMap :: Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape)) -> Ptr TopoDS.Shape -> Acquire (Ptr (List TopoDS.Shape))
findFromKeyShapeListOfShapeMap theMap shape = mkAcquire (wrapException $ rawFindFromKeyShapeListOfShapeMap theMap shape) deleteListOfShape

foreign import capi unsafe "hs_NCollection_IndexedDataMap.h hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_findKey" rawFindKeyShapeListOfShapeMap
    :: Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape))
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

-- | The key at a (1-based) index.
findKeyShapeListOfShapeMap :: Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape)) -> Int -> Acquire (Ptr TopoDS.Shape)
findKeyShapeListOfShapeMap theMap index = mkAcquire (wrapException $ rawFindKeyShapeListOfShapeMap theMap (fromIntegral index)) deleteShape

foreign import capi unsafe "hs_NCollection_IndexedDataMap.h hs_NCollection_IndexedDataMap_TopoDS_Shape_NCollection_List_TopoDS_Shape_findFromIndex" rawFindFromIndexShapeListOfShapeMap
    :: Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape))
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (List TopoDS.Shape))

-- | The list at a (1-based) index.
findFromIndexShapeListOfShapeMap :: Ptr (IndexedDataMap TopoDS.Shape (List TopoDS.Shape)) -> Int -> Acquire (Ptr (List TopoDS.Shape))
findFromIndexShapeListOfShapeMap theMap index = mkAcquire (wrapException $ rawFindFromIndexShapeListOfShapeMap theMap (fromIntegral index)) deleteListOfShape
