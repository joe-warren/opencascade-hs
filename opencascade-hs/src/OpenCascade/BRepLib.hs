{-# LANGUAGE CApiFFI#-}
module OpenCascade.BRepLib 
( orientClosedSolid
, buildCurve3d
, encodeRegularity
) where 

import qualified OpenCascade.GeomAbs.Shape as GeomAbs.Shape
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CDouble (..), CInt (..))
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import Data.Coerce (coerce)
import qualified OpenCascade.TopoDS as TopoDS

foreign import capi unsafe "hs_BRepLib.h hs_BRepLib_orientClosedSolid" rawOrientClosedSolid
    :: Ptr TopoDS.Solid
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (CBool)

orientClosedSolid :: Ptr TopoDS.Solid -> IO Bool
orientClosedSolid s = cBoolToBool <$> wrapException (rawOrientClosedSolid s)


foreign import capi unsafe "hs_BRepLib.h hs_BRepLib_buildCurve3d" rawBuildCurve3d
    :: Ptr TopoDS.Edge
    -> CDouble
    -> CInt
    -> CInt
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CBool

buildCurve3d :: Ptr TopoDS.Edge -> Double -> GeomAbs.Shape.Shape -> Int -> Int -> IO Bool
buildCurve3d edge tolerance continuity maxDegree maxSegment =
    cBoolToBool <$> wrapException (rawBuildCurve3d edge (coerce tolerance) (fromIntegral . fromEnum $ continuity) (fromIntegral maxDegree) (fromIntegral maxSegment))


foreign import capi unsafe "hs_BRepLib.h hs_BRepLib_encodeRegularity" rawEncodeRegularity
    :: Ptr TopoDS.Shape
    -> CDouble
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

encodeRegularity :: Ptr TopoDS.Shape -> Double -> IO ()
encodeRegularity s angularTolerance = wrapException (rawEncodeRegularity s (coerce angularTolerance))