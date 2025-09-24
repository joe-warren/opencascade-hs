module OpenCascade.BOPAlgo.BOP 
( BOP
, new
, addTool
, setOperation
) where

import OpenCascade.BOPAlgo.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.BOPAlgo.Types
import OpenCascade.BOPAlgo.Internal.Destructors (deleteBOP)
import OpenCascade.BOPAlgo.Operation (Operation)
import qualified OpenCascade.TopoDS.Types as TopoDS
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.C (CInt (..))
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> bopAlgoContext)

C.include "<BOPAlgo_BOP.hxx>"
C.include "<TopoDS_Shape.hxx>"
C.include "<BOPAlgo_Operation.hxx>"


new :: Acquire (Ptr BOP)
new =
  let createBOP = [C.throwBlock| BOPAlgo_BOP* {
        return new BOPAlgo_BOP();
      } |]
  in mkAcquire createBOP deleteBOP

addTool :: Ptr BOP -> Ptr TopoDS.Shape -> IO ()
addTool bop shape = [C.throwBlock| void {
  $(BOPAlgo_BOP* bop)->AddTool(*$(TopoDS_Shape* shape));
} |]

setOperation :: Ptr BOP -> Operation -> IO ()
setOperation bop op = do
  let cOp = fromIntegral $ fromEnum op
  [C.throwBlock| void {
    $(BOPAlgo_BOP* bop)->SetOperation(static_cast<BOPAlgo_Operation>($(int cOp)));
  } |]