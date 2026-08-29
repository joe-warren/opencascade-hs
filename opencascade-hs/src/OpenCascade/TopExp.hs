{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopExp
( module OpenCascade.TopExp.Types
, mapShapesAndAncestors
) where

import OpenCascade.TopExp.Types
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.TopAbs as TopAbs
import qualified OpenCascade.TopTools.Types as TopTools
import OpenCascade.TopTools.Internal.Destructors (deleteIndexedDataMapOfShapeListOfShape)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_TopExp.h hs_TopExp_mapShapesAndAncestors" rawMapShapesAndAncestors
    :: Ptr TopoDS.Shape
    -> CInt
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopTools.IndexedDataMapOfShapeListOfShape)

mapShapesAndAncestors :: Ptr TopoDS.Shape -> TopAbs.ShapeEnum -> TopAbs.ShapeEnum -> Acquire (Ptr TopTools.IndexedDataMapOfShapeListOfShape)
mapShapesAndAncestors shape subshapeType ancestorType =
    mkAcquire
        (wrapException $ rawMapShapesAndAncestors shape (fromIntegral . fromEnum $ subshapeType) (fromIntegral . fromEnum $ ancestorType))
        deleteIndexedDataMapOfShapeListOfShape
