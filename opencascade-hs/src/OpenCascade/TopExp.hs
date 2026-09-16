{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopExp
( module OpenCascade.TopExp.Types
, mapShapesAndAncestors
) where

import OpenCascade.TopExp.Types
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.TopAbs as TopAbs
import qualified OpenCascade.NCollection.Types as NCollection
import OpenCascade.NCollection.Internal.Destructors (deleteIndexedDataMapOfShapeListOfShape)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..))
import Data.Acquire (Acquire, mkAcquire)

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
