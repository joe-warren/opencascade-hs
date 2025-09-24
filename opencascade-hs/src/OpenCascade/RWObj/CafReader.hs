module OpenCascade.RWObj.CafReader 
( CafReader
, new
, setSinglePrecision
) where

import OpenCascade.RWObj.Internal.Context
import OpenCascade.RWObj.Types (CafReader)
import OpenCascade.RWObj.Internal.Destructors (deleteCafReader)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool)
import Foreign.C (CBool (..))
import Foreign.Ptr (Ptr)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> rwObjContext)

C.include "<RWObj_CafReader.hxx>"

new :: Acquire (Ptr CafReader)
new =
  let createReader = [C.throwBlock| RWObj_CafReader* {
        return new RWObj_CafReader();
      } |]
  in mkAcquire createReader deleteCafReader

setSinglePrecision :: Ptr CafReader -> Bool -> IO ()
setSinglePrecision reader isSingle = do
  let cIsSingle = boolToCBool isSingle
  [C.throwBlock| void {
    $(RWObj_CafReader* reader)->SetSinglePrecision($(bool cIsSingle));
  } |]