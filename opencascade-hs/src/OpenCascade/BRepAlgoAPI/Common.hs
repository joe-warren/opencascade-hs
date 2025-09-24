module OpenCascade.BRepAlgoAPI.Common
( common
) where

import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> topoDSContext)

C.include "<BRepAlgoAPI_Common.hxx>"
C.include "<TopoDS_Shape.hxx>"

common :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
common shapeA shapeB =
  let createCommon = [C.throwBlock| TopoDS_Shape* {
        BRepAlgoAPI_Common builder(*$(TopoDS_Shape* shapeA), *$(TopoDS_Shape* shapeB));
        return new TopoDS_Shape(builder.Shape());
      } |]
  in mkAcquire createCommon deleteShape