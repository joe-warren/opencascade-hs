{-# LANGUAGE CApiFFI#-}
module OpenCascade.BRepBndLib 
( add
, addOptimal
, addOBB
) where 

import OpenCascade.TopoDS.Types (Shape)
import OpenCascade.Bnd.Types (Box, OBB)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CInt)
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_BRepBndLib.h hs_BRepBndLib_add" rawAdd
    :: Ptr Shape
    -> Ptr Box
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

add :: Ptr Shape  -> Ptr Box -> Bool -> IO ()
add shape box useTriangulation = wrapException $ rawAdd shape box (boolToCBool useTriangulation)

foreign import capi unsafe "hs_BRepBndLib.h hs_BRepBndLib_addOptimal" rawAddOptimal
    :: Ptr Shape
    -> Ptr Box
    -> CBool
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addOptimal :: Ptr Shape  -> Ptr Box -> Bool -- ^ useTriangulation
    -> Bool -- ^ useShapeTolerance
    -> IO ()
addOptimal shape box useTriangulation useShapeTolerance =
    wrapException $ rawAddOptimal shape box (boolToCBool useTriangulation) (boolToCBool useShapeTolerance)

foreign import capi unsafe "hs_BRepBndLib.h hs_BRepBndLib_addOBB" rawAddOBB
    :: Ptr Shape
    -> Ptr OBB
    -> CBool
    -> CBool
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addOBB :: Ptr Shape -> Ptr OBB -> Bool -- ^ isTriangulation used
    -> Bool -- ^ isOptimal
    -> Bool -- ^ is ShapeToleranceUsed
    -> IO ()
addOBB shape obb triangulationUsed optimal shapeToleranceUsed =
    wrapException $ rawAddOBB shape obb (boolToCBool triangulationUsed) (boolToCBool optimal) (boolToCBool shapeToleranceUsed)