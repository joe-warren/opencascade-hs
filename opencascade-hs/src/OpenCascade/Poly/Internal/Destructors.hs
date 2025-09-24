module OpenCascade.Poly.Internal.Destructors
( deleteHandleTriangulation
, deleteTriangle
) where

import OpenCascade.Poly.Internal.Context
import OpenCascade.Handle.Internal.Context (handleContext)
import OpenCascade.Poly.Types 
import OpenCascade.Handle (Handle)
import Foreign.Ptr (Ptr)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> handleContext <> polyContext)

C.include "<Poly_Triangulation.hxx>"
C.include "<Poly_Triangle.hxx>"
C.include "<Standard_Handle.hxx>"

deleteHandleTriangulation :: Ptr (Handle Triangulation) -> IO ()
deleteHandleTriangulation triPtr = [C.throwBlock| void {
  delete $(opencascade::handle<Poly_Triangulation>* triPtr);
} |]

deleteTriangle :: Ptr Triangle -> IO ()
deleteTriangle trianglePtr = [C.throwBlock| void {
  delete $(Poly_Triangle* trianglePtr);
} |]