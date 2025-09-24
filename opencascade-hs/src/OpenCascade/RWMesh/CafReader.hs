module OpenCascade.RWMesh.CafReader
( CafReader
, setDocument
, perform
, singleShape
, setFileLengthUnit
) where

import OpenCascade.RWMesh.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.TDocStd.Internal.Context (tDocStdContext)
import OpenCascade.Message.Internal.Context (messageContext)
import OpenCascade.Handle.Internal.Context (handleContext)
import OpenCascade.RWMesh.Types (CafReader)
import qualified OpenCascade.TDocStd.Types as TDocStd
import qualified OpenCascade.Message.Types as Message
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Bool (cBoolToBool)
import Foreign.C (CBool (..), CDouble (..))
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> tDocStdContext <> messageContext <> handleContext <> rwMeshContext)

C.include "<RWMesh_CafReader.hxx>"
C.include "<TopoDS_Shape.hxx>"
C.include "<TDocStd_Document.hxx>"
C.include "<Message_ProgressRange.hxx>"
C.include "<Standard_Handle.hxx>"

setDocument :: Ptr CafReader -> Ptr (Handle TDocStd.Document) -> IO ()
setDocument reader doc = [C.throwBlock| void {
  $(RWMesh_CafReader* reader)->SetDocument(*$(opencascade::handle<TDocStd_Document>* doc));
} |]

setFileLengthUnit :: Ptr CafReader -> Double -> IO ()
setFileLengthUnit reader unit = do
  let cUnit = realToFrac unit
  [C.throwBlock| void {
    $(RWMesh_CafReader* reader)->SetFileLengthUnit($(double cUnit));
  } |]

perform :: Ptr CafReader -> String -> Ptr Message.ProgressRange -> IO Bool
perform reader filename progress = do
  result <- withCString filename $ \cFilename -> [C.throwBlock| bool {
    return $(RWMesh_CafReader* reader)->Perform($(char* cFilename), *$(Message_ProgressRange* progress));
  } |]
  return (cBoolToBool result)

singleShape :: Ptr CafReader -> Acquire (Ptr TopoDS.Shape)
singleShape reader =
  let createShape = [C.throwBlock| TopoDS_Shape* {
        return new TopoDS_Shape($(RWMesh_CafReader* reader)->SingleShape());
      } |]
  in mkAcquire createShape deleteShape

