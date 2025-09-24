module OpenCascade.StlAPI.Writer
( Writer
, new
, setAsciiMode
, write
) where

import OpenCascade.StlAPI.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.StlAPI.Types (Writer)
import OpenCascade.StlAPI.Internal.Destructors (deleteWriter)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.C
import Foreign.Ptr
import Data.Acquire
import OpenCascade.Internal.Bool (boolToCBool, cBoolToBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> stlAPIContext)

C.include "<StlAPI_Writer.hxx>"
C.include "<TopoDS_Shape.hxx>"

new :: Acquire (Ptr Writer)
new =
  let createWriter = [C.throwBlock| StlAPI_Writer* {
        return new StlAPI_Writer();
      } |]
  in mkAcquire createWriter deleteWriter

-- TODO: Need to research correct method name for ASCII mode
setAsciiMode :: Ptr Writer -> Bool -> IO ()
setAsciiMode writer mode = do
  let cMode = boolToCBool mode
  [C.throwBlock| void {
    $(StlAPI_Writer* writer)->ASCIIMode() = ($(bool cMode));
  } |]

write :: Ptr Writer -> Ptr TopoDS.Shape -> String -> IO Bool
write writer shape filename = do
  result <- withCString filename $ \cFilename -> [C.throwBlock| bool {
    return $(StlAPI_Writer* writer)->Write(*$(TopoDS_Shape* shape), $(char* cFilename));
  } |]
  return (cBoolToBool result)