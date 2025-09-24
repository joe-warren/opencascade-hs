module OpenCascade.BOPAlgo.Internal.Destructors 
( deleteBuilder
, deleteBOP
) where

import OpenCascade.BOPAlgo.Internal.Context
import OpenCascade.BOPAlgo.Types
import Foreign.Ptr
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> bopAlgoContext)

C.include "<BOPAlgo_Builder.hxx>"
C.include "<BOPAlgo_BOP.hxx>"

deleteBuilder :: Ptr Builder -> IO ()
deleteBuilder builderPtr = [C.throwBlock| void {
  delete $(BOPAlgo_Builder* builderPtr);
} |]

deleteBOP :: Ptr BOP -> IO ()
deleteBOP bopPtr = [C.throwBlock| void {
  delete $(BOPAlgo_BOP* bopPtr);
} |]
