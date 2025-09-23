module OpenCascade.GP.GTrsf
( GTrsf
, new
, setValue
, setForm
) where

import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Context
import OpenCascade.GP.Internal.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext)

C.include "<gp_GTrsf.hxx>"

new :: Acquire (Ptr GTrsf)
new = mkAcquire createGTrsf deleteGTrsf
  where
    createGTrsf = [C.throwBlock| gp_GTrsf* {
      return new gp_GTrsf();
    } |]

setValue :: Ptr GTrsf -> Int -> Int -> Double -> IO ()
setValue trsf row column value = do
  let cRow = fromIntegral row
      cColumn = fromIntegral column
      cValue = realToFrac value
  [C.throwBlock| void {
    $(gp_GTrsf* trsf)->SetValue($(int cRow), $(int cColumn), $(double cValue));
  } |]

setForm :: Ptr GTrsf -> IO ()
setForm trsf = [C.throwBlock| void {
  $(gp_GTrsf* trsf)->SetForm();
} |] 
