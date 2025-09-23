module OpenCascade.TopoDS.Internal.Destructors 
( deleteShape
, deleteBuilder
) where

import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Context
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> topoDSContext)

C.include "<TopoDS_Shape.hxx>"
C.include "<TopoDS_Builder.hxx>"

deleteShape :: Ptr Shape -> IO ()
deleteShape shape = [C.throwBlock| void {
  delete $(TopoDS_Shape* shape);
} |]

deleteBuilder :: Ptr Builder -> IO ()
deleteBuilder builder = [C.throwBlock| void {
  delete $(TopoDS_Builder* builder);
} |]

