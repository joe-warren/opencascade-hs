module OpenCascade.RWGltf.CafWriter
( CafWriter
, new
, perform
) where

import OpenCascade.RWGltf.Internal.Context
import OpenCascade.TDocStd.Internal.Context (tDocStdContext)
import OpenCascade.TColStd.Internal.Context (tColStdContext)
import OpenCascade.Message.Internal.Context (messageContext)
import OpenCascade.RWGltf.Types (CafWriter)
import OpenCascade.RWGltf.Internal.Destructors (deleteCafWriter)
import Foreign.Ptr (Ptr)
import Foreign.C.String (withCString)
import Foreign.C (CBool (..))
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Handle (Handle)
import OpenCascade.Handle.Internal.Context (handleContext)
import qualified OpenCascade.TDocStd.Types as TDocStd
import qualified OpenCascade.TColStd.Types as TColStd
import qualified OpenCascade.Message.Types as Message
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> handleContext <> tDocStdContext <> tColStdContext <> messageContext <> rwGltfContext)

C.include "<RWGltf_CafWriter.hxx>"
C.include "<TDocStd_Document.hxx>"
C.include "<TColStd_IndexedDataMapOfStringString.hxx>"
C.include "<Message_ProgressRange.hxx>"
C.include "<Standard_Handle.hxx>"

new :: String -> Bool -> Acquire (Ptr CafWriter)
new filepath binary =
  let cBinary = boolToCBool binary
      createWriter = withCString filepath $ \cFilePath -> [C.throwBlock| RWGltf_CafWriter* {
        return new RWGltf_CafWriter($(char* cFilePath), $(bool cBinary));
      } |]
  in mkAcquire createWriter deleteCafWriter

perform :: Ptr CafWriter -> Ptr (Handle TDocStd.Document) -> Ptr (TColStd.IndexedDataMapOfStringString) -> Ptr (Message.ProgressRange) -> IO ()
perform writer document metadata progress = [C.throwBlock| void {
  $(RWGltf_CafWriter* writer)->Perform(
    *$(opencascade::handle<TDocStd_Document>* document),
    *$(TColStd_IndexedDataMapOfStringString* metadata),
    *$(Message_ProgressRange* progress)
  );
} |]