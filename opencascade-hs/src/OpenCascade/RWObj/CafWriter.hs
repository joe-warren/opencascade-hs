module OpenCascade.RWObj.CafWriter
( CafWriter
, new
, perform
) where

import OpenCascade.RWObj.Internal.Context
import OpenCascade.TDocStd.Internal.Context (tDocStdContext)
import OpenCascade.TColStd.Internal.Context (tColStdContext)
import OpenCascade.Message.Internal.Context (messageContext)
import OpenCascade.Handle.Internal.Context (handleContext)
import OpenCascade.RWObj.Types (CafWriter)
import OpenCascade.RWObj.Internal.Destructors (deleteCafWriter)
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, withCString)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Handle (Handle)
import qualified OpenCascade.TDocStd.Types as TDocStd
import qualified OpenCascade.TColStd.Types as TColStd
import qualified OpenCascade.Message.Types as Message
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> tDocStdContext <> tColStdContext <> messageContext <> handleContext <> rwObjContext)

C.include "<RWObj_CafWriter.hxx>"
C.include "<TDocStd_Document.hxx>"
C.include "<TColStd_IndexedDataMapOfStringString.hxx>"
C.include "<Message_ProgressRange.hxx>"
C.include "<Standard_Handle.hxx>"

new :: String -> Acquire (Ptr CafWriter)
new filepath =
  let createWriter = withCString filepath $ \cFilepath -> [C.throwBlock| RWObj_CafWriter* {
        return new RWObj_CafWriter($(char* cFilepath));
      } |]
  in mkAcquire createWriter deleteCafWriter

perform :: Ptr CafWriter -> Ptr (Handle TDocStd.Document) -> Ptr (TColStd.IndexedDataMapOfStringString) -> Ptr (Message.ProgressRange) -> IO ()
perform writer doc metadata progress = [C.throwBlock| void {
  $(RWObj_CafWriter* writer)->Perform(*$(opencascade::handle<TDocStd_Document>* doc), 
                                      *$(TColStd_IndexedDataMapOfStringString* metadata), 
                                      *$(Message_ProgressRange* progress));
} |]