module OpenCascade.RWGltf.CafReader 
( CafReader
, new
, setDoublePrecision
) where

import OpenCascade.RWGltf.Internal.Context
import OpenCascade.RWGltf.Types (CafReader)
import OpenCascade.RWGltf.Internal.Destructors (deleteCafReader)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool)
import Foreign.C (CBool (..))
import Foreign.Ptr (Ptr)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> rwGltfContext)

C.include "<RWGltf_CafReader.hxx>"

new :: Acquire (Ptr CafReader)
new =
  let createReader = [C.throwBlock| RWGltf_CafReader* {
        return new RWGltf_CafReader();
      } |]
  in mkAcquire createReader deleteCafReader

setDoublePrecision :: Ptr CafReader -> Bool -> IO ()
setDoublePrecision reader isDouble = do
  let cIsDouble = boolToCBool isDouble
  [C.throwBlock| void {
    $(RWGltf_CafReader* reader)->SetDoublePrecision($(bool cIsDouble));
  } |]