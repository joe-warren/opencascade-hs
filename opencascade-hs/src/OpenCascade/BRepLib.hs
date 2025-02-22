{-# LANGUAGE CApiFFI#-}
module OpenCascade.BRepLib 
( orientClosedSolid
, buildCurve3d
) where 

import OpenCascade.TopoDS.Types (Solid, Edge)
import qualified OpenCascade.GeomAbs.Shape as GeomAbs.Shape
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CDouble (..), CInt (..))
import OpenCascade.Internal.Bool (cBoolToBool)
import Data.Coerce (coerce)

foreign import capi unsafe "hs_BRepLib.h hs_BRepLib_orientClosedSolid" rawOrientClosedSolid ::  Ptr Solid -> IO (CBool)

orientClosedSolid :: Ptr Solid -> IO Bool
orientClosedSolid s = cBoolToBool <$> rawOrientClosedSolid s


foreign import capi unsafe "hs_BRepLib.h hs_BRepLib_buildCurve3d" rawBuildCurve3d ::  Ptr Edge -> CDouble -> CInt -> CInt -> CInt -> IO CBool

buildCurve3d :: Ptr Edge -> Double -> GeomAbs.Shape.Shape -> Int -> Int -> IO Bool
buildCurve3d edge tolerance continuity maxDegree maxSegment = 
    cBoolToBool <$> rawBuildCurve3d edge (coerce tolerance) (fromIntegral . fromEnum $ continuity) (fromIntegral maxDegree) (fromIntegral maxSegment)

