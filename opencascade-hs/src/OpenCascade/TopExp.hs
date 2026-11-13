{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopExp
( module OpenCascade.TopExp.Types
, mapShapesAndAncestors
, firstVertex
, lastVertex
) where

import OpenCascade.TopExp.Types
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified OpenCascade.TopAbs as TopAbs
import qualified OpenCascade.NCollection.Types as NCollection
import OpenCascade.Inheritance (upcast)
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Internal.Exception (wrapException)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CBool (..))

foreign import capi unsafe "hs_TopExp.h hs_TopExp_mapShapesAndAncestors" rawMapShapesAndAncestors
    :: Ptr TopoDS.Shape
    -> CInt
    -> CInt
    -> (Ptr (NCollection.IndexedDataMap TopoDS.Shape (NCollection.List TopoDS.Shape)))
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

mapShapesAndAncestors 
    :: Ptr TopoDS.Shape
    -> TopAbs.ShapeEnum
    -> TopAbs.ShapeEnum
    -> (Ptr (NCollection.IndexedDataMap TopoDS.Shape (NCollection.List TopoDS.Shape)))
    -> IO () 
mapShapesAndAncestors shape subshapeType ancestorType theMap =
    wrapException $ rawMapShapesAndAncestors
        shape 
        (fromIntegral . fromEnum $ subshapeType)
        (fromIntegral . fromEnum $ ancestorType)
        theMap

foreign import capi unsafe "hs_TopExp.h hs_TopExp_firstVertex" rawFirstVertex
    :: Ptr TopoDS.Edge
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Vertex)

firstVertex :: Ptr TopoDS.Edge -> Bool -> Acquire (Ptr TopoDS.Vertex)
firstVertex edge cumOri =
    mkAcquire
        (wrapException $ rawFirstVertex edge (boolToCBool cumOri))
        (TopoDS.Destructors.deleteShape . upcast)

foreign import capi unsafe "hs_TopExp.h hs_TopExp_lastVertex" rawLastVertex
    :: Ptr TopoDS.Edge
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Vertex)

lastVertex :: Ptr TopoDS.Edge -> Bool -> Acquire (Ptr TopoDS.Vertex)
lastVertex edge cumOri =
    mkAcquire
        (wrapException $ rawLastVertex edge (boolToCBool cumOri))
        (TopoDS.Destructors.deleteShape . upcast)
