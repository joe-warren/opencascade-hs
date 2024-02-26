{-# LANGUAGE CApiFFI#-}
module OpenCascade.BRepBndLib 
( add
, addOptimal
, addOBB
) where 

import OpenCascade.TopoDS.Types (Shape)
import OpenCascade.Bnd.Types (Box, OBB)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))
import OpenCascade.Internal.Bool (boolToCBool)

foreign import capi unsafe "hs_BRepBndLib.h hs_BRepBndLib_add" rawAdd ::  Ptr Shape -> Ptr Box -> CBool -> IO ()

add :: Ptr Shape  -> Ptr Box -> Bool -> IO ()
add shape box useTriangulation = rawAdd shape box (boolToCBool useTriangulation)

foreign import capi unsafe "hs_BRepBndLib.h hs_BRepBndLib_addOptimal" rawAddOptimal ::  Ptr Shape -> Ptr Box -> CBool -> CBool -> IO ()

addOptimal :: Ptr Shape  -> Ptr Box -> Bool -- ^ useTriangulation
    -> Bool -- ^ useShapeTolerance
    -> IO ()
addOptimal shape box useTriangulation useShapeTolerance = 
    rawAddOptimal shape box (boolToCBool useTriangulation) (boolToCBool useShapeTolerance)

foreign import capi unsafe "hs_BRepBndLib.h hs_BRepBndLib_addOBB" rawAddOBB ::  Ptr Shape -> Ptr OBB -> CBool -> CBool -> CBool -> IO ()

addOBB :: Ptr Shape -> Ptr OBB -> Bool -- ^ isTriangulation used
    -> Bool -- ^ isOptimal
    -> Bool -- ^ is ShapeToleranceUsed
    -> IO ()
addOBB shape obb triangulationUsed optimal shapeToleranceUsed = 
    rawAddOBB shape obb (boolToCBool triangulationUsed) (boolToCBool optimal) (boolToCBool shapeToleranceUsed)