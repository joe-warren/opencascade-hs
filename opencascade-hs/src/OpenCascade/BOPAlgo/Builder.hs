module OpenCascade.BOPAlgo.Builder
( Builder
, new
, addArgument
, setRunParallel
, shape
, perform
) where

import OpenCascade.BOPAlgo.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.BOPAlgo.Types
import OpenCascade.BOPAlgo.Internal.Destructors (deleteBuilder)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Internal.Bool (boolToCBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> bopAlgoContext)

C.include "<BOPAlgo_Builder.hxx>"
C.include "<TopoDS_Shape.hxx>"

new :: Acquire (Ptr Builder)
new =
  let createBuilder = [C.throwBlock| BOPAlgo_Builder* {
        return new BOPAlgo_Builder();
      } |]
  in mkAcquire createBuilder deleteBuilder

addArgument :: Ptr Builder -> Ptr TopoDS.Shape -> IO ()
addArgument builder shape = [C.throwBlock| void {
  $(BOPAlgo_Builder* builder)->AddArgument(*$(TopoDS_Shape* shape));
} |]

setRunParallel :: Ptr Builder -> Bool -> IO ()
setRunParallel builder runParallel = do
  let cRunParallel = boolToCBool runParallel
  [C.throwBlock| void {
    $(BOPAlgo_Builder* builder)->SetRunParallel($(bool cRunParallel));
  } |]

shape :: Ptr Builder -> Acquire (Ptr TopoDS.Shape)
shape builder =
  let createShape = [C.throwBlock| TopoDS_Shape* {
        return new TopoDS_Shape($(BOPAlgo_Builder* builder)->Shape());
      } |]
  in mkAcquire createShape deleteShape

perform :: Ptr Builder -> IO ()
perform builder = [C.throwBlock| void {
  $(BOPAlgo_Builder* builder)->Perform();
} |]

