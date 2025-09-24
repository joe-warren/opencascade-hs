module OpenCascade.STEPControl.Reader
( Reader
, new
) where

import OpenCascade.STEPControl.Internal.Context
import OpenCascade.STEPControl.Types (Reader)
import OpenCascade.STEPControl.Internal.Destructors (deleteReader)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> stepControlContext)

C.include "<STEPControl_Reader.hxx>"

new :: Acquire (Ptr Reader)
new =
  let createReader = [C.throwBlock| STEPControl_Reader* {
        return new STEPControl_Reader();
      } |]
  in mkAcquire createReader deleteReader