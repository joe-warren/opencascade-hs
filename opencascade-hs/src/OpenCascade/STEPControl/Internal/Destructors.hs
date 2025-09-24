module OpenCascade.STEPControl.Internal.Destructors 
( deleteWriter
, deleteReader
) where

import OpenCascade.STEPControl.Internal.Context
import OpenCascade.STEPControl.Types
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> stepControlContext)

C.include "<STEPControl_Writer.hxx>"
C.include "<STEPControl_Reader.hxx>"

deleteWriter :: Ptr Writer -> IO ()
deleteWriter writerPtr = [C.throwBlock| void {
  delete $(STEPControl_Writer* writerPtr);
} |]

deleteReader :: Ptr Reader -> IO ()
deleteReader readerPtr = [C.throwBlock| void {
  delete $(STEPControl_Reader* readerPtr);
} |]