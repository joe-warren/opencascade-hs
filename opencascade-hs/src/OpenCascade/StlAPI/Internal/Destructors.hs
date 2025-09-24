module OpenCascade.StlAPI.Internal.Destructors 
( deleteWriter
, deleteReader
) where

import OpenCascade.StlAPI.Internal.Context
import OpenCascade.StlAPI.Types
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> stlAPIContext)

C.include "<StlAPI_Writer.hxx>"
C.include "<StlAPI_Reader.hxx>"

deleteWriter :: Ptr Writer -> IO ()
deleteWriter writerPtr = [C.throwBlock| void {
  delete $(StlAPI_Writer* writerPtr);
} |]

deleteReader :: Ptr Reader -> IO ()
deleteReader readerPtr = [C.throwBlock| void {
  delete $(StlAPI_Reader* readerPtr);
} |]