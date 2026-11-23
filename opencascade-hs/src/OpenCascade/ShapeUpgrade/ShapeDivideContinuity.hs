{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeUpgrade.ShapeDivideContinuity
( ShapeDivideContinuity
, fromShape
, setTolerance
, setTolerance2d
, setBoundaryCriterion
, setPCurveCriterion
, setSurfaceCriterion
) where

import OpenCascade.ShapeUpgrade.Types (ShapeDivideContinuity)
import OpenCascade.ShapeUpgrade.Internal.Destructors (deleteShapeDivideContinuity)
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.GeomAbs.Shape as GeomAbs.Shape
import OpenCascade.Internal.Exception (wrapException)
import Data.Acquire (Acquire, mkAcquire)
import Data.Coerce (coerce)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CDouble (..))

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivideContinuity.h hs_new_ShapeUpgrade_ShapeDivideContinuity_fromShape" rawFromShape
    :: Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr ShapeDivideContinuity)

fromShape :: Ptr TopoDS.Shape -> Acquire (Ptr ShapeDivideContinuity)
fromShape s = mkAcquire (wrapException $ rawFromShape s) deleteShapeDivideContinuity

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivideContinuity.h hs_ShapeUpgrade_ShapeDivideContinuity_setTolerance" rawSetTolerance :: Ptr ShapeDivideContinuity -> CDouble -> IO ()

setTolerance :: Ptr ShapeDivideContinuity -> Double -> IO ()
setTolerance = coerce rawSetTolerance

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivideContinuity.h hs_ShapeUpgrade_ShapeDivideContinuity_setTolerance2d" rawSetTolerance2d :: Ptr ShapeDivideContinuity -> CDouble -> IO ()

setTolerance2d :: Ptr ShapeDivideContinuity -> Double -> IO ()
setTolerance2d = coerce rawSetTolerance2d

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivideContinuity.h hs_ShapeUpgrade_ShapeDivideContinuity_setBoundaryCriterion" rawSetBoundaryCriterion :: Ptr ShapeDivideContinuity -> CInt -> IO ()

setBoundaryCriterion :: Ptr ShapeDivideContinuity -> GeomAbs.Shape.Shape -> IO ()
setBoundaryCriterion divide = rawSetBoundaryCriterion divide . fromIntegral . fromEnum

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivideContinuity.h hs_ShapeUpgrade_ShapeDivideContinuity_setPCurveCriterion" rawSetPCurveCriterion :: Ptr ShapeDivideContinuity -> CInt -> IO ()

setPCurveCriterion :: Ptr ShapeDivideContinuity -> GeomAbs.Shape.Shape -> IO ()
setPCurveCriterion divide = rawSetPCurveCriterion divide . fromIntegral . fromEnum

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivideContinuity.h hs_ShapeUpgrade_ShapeDivideContinuity_setSurfaceCriterion" rawSetSurfaceCriterion :: Ptr ShapeDivideContinuity -> CInt -> IO ()

setSurfaceCriterion :: Ptr ShapeDivideContinuity -> GeomAbs.Shape.Shape -> IO ()
setSurfaceCriterion divide = rawSetSurfaceCriterion divide . fromIntegral . fromEnum
