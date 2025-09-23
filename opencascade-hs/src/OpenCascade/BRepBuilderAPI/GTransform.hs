module OpenCascade.BRepBuilderAPI.GTransform 
( gtransform
) where

import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Bool
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepBuilderAPIContext)

C.include "<BRepBuilderAPI_GTransform.hxx>"
C.include "<gp_GTrsf.hxx>" 

gtransform :: Ptr TopoDS.Shape -> Ptr GP.GTrsf -> Bool -> Acquire (Ptr TopoDS.Shape)
gtransform shape gtrsf copy = mkAcquire createGTransform TopoDS.Destructors.deleteShape
  where
    createGTransform = 
      let cCopy = boolToCBool copy
      in [C.throwBlock| TopoDS_Shape* {
        BRepBuilderAPI_GTransform transformer(*$(TopoDS_Shape* shape), *$(gp_GTrsf* gtrsf), $(bool cCopy));
        return new TopoDS_Shape(transformer.Shape());
      } |]
