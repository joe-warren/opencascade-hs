module OpenCascade.Font.Internal.Destructors
( deleteBRepFont
, deleteBRepTextBuilder
) where

import OpenCascade.Font.Internal.Context
import OpenCascade.Font.Types
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> fontContext)

C.include "<Font_BRepFont.hxx>"
C.include "<Font_BRepTextBuilder.hxx>"

deleteBRepFont :: Ptr BRepFont -> IO ()
deleteBRepFont fontPtr = [C.throwBlock| void {
  delete $(Font_BRepFont* fontPtr);
} |]

deleteBRepTextBuilder :: Ptr BRepTextBuilder -> IO ()
deleteBRepTextBuilder builderPtr = [C.throwBlock| void {
  delete $(Font_BRepTextBuilder* builderPtr);
} |]