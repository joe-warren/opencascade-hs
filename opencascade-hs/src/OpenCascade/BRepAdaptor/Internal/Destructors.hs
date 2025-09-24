module OpenCascade.BRepAdaptor.Internal.Destructors
( deleteCurve
) where

import OpenCascade.BRepAdaptor.Internal.Context
import OpenCascade.BRepAdaptor.Types (Curve)
import Foreign.Ptr
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> brepAdaptorContext)

C.include "<BRepAdaptor_Curve.hxx>"

deleteCurve :: Ptr Curve -> IO ()
deleteCurve curvePtr = [C.throwBlock| void {
  delete $(BRepAdaptor_Curve* curvePtr);
} |]

