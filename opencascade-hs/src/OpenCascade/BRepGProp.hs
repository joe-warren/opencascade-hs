module OpenCascade.BRepGProp (volumeProperties) where

import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.TopoDS.Types (Shape)
import OpenCascade.GProp.Types (GProps)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))
import OpenCascade.Internal.Bool (boolToCBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> C.cppTypePairs [("GProp_GProps", [t| GProps |])])

C.include "<BRepGProp.hxx>"
C.include "<TopoDS_Shape.hxx>"
C.include "<GProp_GProps.hxx>"

volumeProperties :: Ptr Shape -> Ptr GProps -> Bool -> Bool -> Bool -> IO ()
volumeProperties shape props onlyClosed skipShared useTriangulation = do
  let cOnlyClosed = boolToCBool onlyClosed
      cSkipShared = boolToCBool skipShared
      cUseTriangulation = boolToCBool useTriangulation
  [C.throwBlock| void {
    BRepGProp::VolumeProperties(*$(TopoDS_Shape* shape), *$(GProp_GProps* props), $(bool cOnlyClosed), $(bool cSkipShared), $(bool cUseTriangulation));
  } |]
 