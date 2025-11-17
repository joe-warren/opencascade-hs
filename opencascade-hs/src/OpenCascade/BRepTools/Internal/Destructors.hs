module OpenCascade.BRepTools.Internal.Destructors
( deleteWireExplorer
) where

import OpenCascade.BRepTools.Internal.Context
import OpenCascade.BRepTools.Types
import Foreign.Ptr
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> brepToolsContext)

C.include "<BRepTools_WireExplorer.hxx>"

deleteWireExplorer :: Ptr WireExplorer -> IO ()
deleteWireExplorer explorerPtr = [C.throwBlock| void {
  delete $(BRepTools_WireExplorer* explorerPtr);
} |]