module OpenCascade.BRepFilletAPI.Internal.Destructors
( deleteMakeFillet
, deleteMakeChamfer
) where

import OpenCascade.BRepFilletAPI.Internal.Context
import OpenCascade.BRepFilletAPI.Types
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> brepFilletAPIContext)

C.include "<BRepFilletAPI_MakeFillet.hxx>"
C.include "<BRepFilletAPI_MakeChamfer.hxx>"

deleteMakeFillet :: Ptr MakeFillet -> IO ()
deleteMakeFillet filletPtr = [C.throwBlock| void {
  delete $(BRepFilletAPI_MakeFillet* filletPtr);
} |]

deleteMakeChamfer :: Ptr MakeChamfer -> IO ()
deleteMakeChamfer chamferPtr = [C.throwBlock| void {
  delete $(BRepFilletAPI_MakeChamfer* chamferPtr);
} |]


