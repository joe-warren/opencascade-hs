module OpenCascade.BRepLib 
( orientClosedSolid
, buildCurve3d
) where 

import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.TopoDS.Types (Solid, Edge)
import qualified OpenCascade.GeomAbs.Shape as GeomAbs.Shape
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CDouble (..), CInt (..))
import OpenCascade.Internal.Bool (cBoolToBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext)

C.include "<BRepLib.hxx>"
C.include "<TopoDS_Solid.hxx>"
C.include "<TopoDS_Edge.hxx>"

orientClosedSolid :: Ptr Solid -> IO Bool
orientClosedSolid solid = do
  result <- [C.throwBlock| bool {
    return BRepLib::OrientClosedSolid(*$(TopoDS_Solid* solid));
  } |]
  return (cBoolToBool result)

buildCurve3d :: Ptr Edge -> Double -> GeomAbs.Shape.Shape -> Int -> Int -> IO Bool
buildCurve3d edge tolerance continuity maxDegree maxSegment = do
  let cTolerance = realToFrac tolerance
      cContinuity = fromIntegral $ fromEnum continuity
      cMaxDegree = fromIntegral maxDegree
      cMaxSegment = fromIntegral maxSegment
  result <- [C.throwBlock| bool {
    return BRepLib::BuildCurve3d(*$(TopoDS_Edge* edge), $(double cTolerance), static_cast<GeomAbs_Shape>($(int cContinuity)), $(int cMaxDegree), $(int cMaxSegment));
  } |]
  return (cBoolToBool result)

