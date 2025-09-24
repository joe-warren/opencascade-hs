module OpenCascade.StlAPI.Reader
( Reader
, new
, read
) where

import Prelude hiding (read)
import OpenCascade.StlAPI.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.StlAPI.Types (Reader)
import OpenCascade.StlAPI.Internal.Destructors (deleteReader)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.C
import Foreign.Ptr
import Data.Acquire
import OpenCascade.Internal.Bool (cBoolToBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> stlAPIContext)

C.include "<StlAPI_Reader.hxx>"
C.include "<TopoDS_Shape.hxx>"

new :: Acquire (Ptr Reader)
new =
  let createReader = [C.throwBlock| StlAPI_Reader* {
        return new StlAPI_Reader();
      } |]
  in mkAcquire createReader deleteReader

read :: Ptr Reader -> Ptr TopoDS.Shape -> String -> IO Bool
read reader shape filename = do
  result <- withCString filename $ \cFilename -> [C.throwBlock| bool {
    return $(StlAPI_Reader* reader)->Read(*$(TopoDS_Shape* shape), $(char* cFilename));
  } |]
  return (cBoolToBool result)