module OpenCascade.RWGltf.CafWriter
( CafWriter
, new
, perform
) where

import OpenCascade.RWGltf.Internal.Context
import OpenCascade.RWGltf.Types (CafWriter)
import OpenCascade.RWGltf.Internal.Destructors (deleteCafWriter)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.String (withCString)
import Foreign.C (CBool (..))
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Handle (Handle)
import qualified OpenCascade.TDocStd.Types as TDocStd
import qualified OpenCascade.TColStd.Types as TColStd
import qualified OpenCascade.Message.Types as Message
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> rwGltfContext)

C.include "<RWGltf_CafWriter.hxx>"
C.include "<TDocStd_Document.hxx>"
C.include "<Standard_Handle.hxx>"

new :: String -> Bool -> Acquire (Ptr CafWriter)
new filepath binary =
  let cBinary = boolToCBool binary
      createWriter = withCString filepath $ \cFilePath -> [C.throwBlock| RWGltf_CafWriter* {
        return new RWGltf_CafWriter($(char* cFilePath), $(bool cBinary));
      } |]
  in mkAcquire createWriter deleteCafWriter

perform :: Ptr CafWriter -> Ptr (Handle TDocStd.Document) -> Ptr (TColStd.IndexedDataMapOfStringString) -> Ptr (Message.ProgressRange) -> IO ()
perform writer document metadata progress =
  let cDocument = castPtr document
      cMetadata = castPtr metadata  
      cProgress = castPtr progress
  in [C.throwBlock| void {
    $(RWGltf_CafWriter* writer)->Perform(
      *static_cast<opencascade::handle<TDocStd_Document>*>($(void* cDocument)),
      *static_cast<TColStd_IndexedDataMapOfStringString*>($(void* cMetadata)),
      *static_cast<Message_ProgressRange*>($(void* cProgress))
    );
  } |]