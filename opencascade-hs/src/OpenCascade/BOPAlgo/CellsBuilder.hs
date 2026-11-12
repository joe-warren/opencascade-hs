{-# LANGUAGE CApiFFI #-}
module OpenCascade.BOPAlgo.CellsBuilder
( CellsBuilder
, new
, addToResult
, addAllToResult
, removeFromResult
, removeAllFromResult
, removeInternalBoundaries
, getAllParts
, makeContainers
) where

import OpenCascade.BOPAlgo.Types
import OpenCascade.BOPAlgo.Internal.Destructors (deleteCellsBuilder)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Exception (wrapException)
import OpenCascade.Internal.Bool (boolToCBool)
import qualified OpenCascade.NCollection.Types as NCollection
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CInt (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_BOPAlgo_CellsBuilder.h hs_new_BOPAlgo_CellsBuilder" rawNew :: IO (Ptr CellsBuilder)

new :: Acquire (Ptr CellsBuilder)
new = mkAcquire rawNew deleteCellsBuilder

foreign import capi unsafe "hs_BOPAlgo_CellsBuilder.h hs_BOPAlgo_CellsBuilder_AddToResult" rawAddToResult
    :: Ptr CellsBuilder
    -> Ptr (NCollection.List TopoDS.Shape)
    -> Ptr (NCollection.List TopoDS.Shape)
    -> CInt
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addToResult :: Ptr CellsBuilder -> Ptr (NCollection.List TopoDS.Shape) -> Ptr (NCollection.List TopoDS.Shape) -> Int -> Bool -> IO ()
addToResult builder toTake toAvoid material update =
    wrapException $ rawAddToResult builder toTake toAvoid (fromIntegral material) (boolToCBool update)

foreign import capi unsafe "hs_BOPAlgo_CellsBuilder.h hs_BOPAlgo_CellsBuilder_AddAllToResult" rawAddAllToResult
    :: Ptr CellsBuilder
    -> CInt
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addAllToResult :: Ptr CellsBuilder -> Int -> Bool -> IO ()
addAllToResult builder material update =
    wrapException $ rawAddAllToResult builder (fromIntegral material) (boolToCBool update)

foreign import capi unsafe "hs_BOPAlgo_CellsBuilder.h hs_BOPAlgo_CellsBuilder_RemoveFromResult" rawRemoveFromResult
    :: Ptr CellsBuilder
    -> Ptr (NCollection.List TopoDS.Shape)
    -> Ptr (NCollection.List TopoDS.Shape)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

removeFromResult :: Ptr CellsBuilder -> Ptr (NCollection.List TopoDS.Shape) -> Ptr (NCollection.List TopoDS.Shape) -> IO ()
removeFromResult builder toTake toAvoid = wrapException $ rawRemoveFromResult builder toTake toAvoid

foreign import capi unsafe "hs_BOPAlgo_CellsBuilder.h hs_BOPAlgo_CellsBuilder_RemoveAllFromResult" rawRemoveAllFromResult
    :: Ptr CellsBuilder
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

removeAllFromResult :: Ptr CellsBuilder -> IO ()
removeAllFromResult builder = wrapException $ rawRemoveAllFromResult builder

foreign import capi unsafe "hs_BOPAlgo_CellsBuilder.h hs_BOPAlgo_CellsBuilder_RemoveInternalBoundaries" rawRemoveInternalBoundaries
    :: Ptr CellsBuilder
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

removeInternalBoundaries :: Ptr CellsBuilder -> IO ()
removeInternalBoundaries builder = wrapException $ rawRemoveInternalBoundaries builder

foreign import capi unsafe "hs_BOPAlgo_CellsBuilder.h hs_BOPAlgo_CellsBuilder_GetAllParts" rawGetAllParts
    :: Ptr CellsBuilder
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

getAllParts :: Ptr CellsBuilder -> Acquire (Ptr TopoDS.Shape)
getAllParts builder = mkAcquire (wrapException $ rawGetAllParts builder) deleteShape

foreign import capi unsafe "hs_BOPAlgo_CellsBuilder.h hs_BOPAlgo_CellsBuilder_MakeContainers" rawMakeContainers
    :: Ptr CellsBuilder
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

makeContainers :: Ptr CellsBuilder -> IO ()
makeContainers builder = wrapException $ rawMakeContainers builder
