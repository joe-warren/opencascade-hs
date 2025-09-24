module OpenCascade.BRepAlgoAPI.Cut
( cut
) where

import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> topoDSContext)

C.include "<BRepAlgoAPI_Cut.hxx>"
C.include "<TopoDS_Shape.hxx>"

cut :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
cut shapeA shapeB =
  let createCut = [C.throwBlock| TopoDS_Shape* {
        BRepAlgoAPI_Cut builder(*$(TopoDS_Shape* shapeA), *$(TopoDS_Shape* shapeB));
        return new TopoDS_Shape(builder.Shape());
      } |]
  in mkAcquire createCut deleteShape