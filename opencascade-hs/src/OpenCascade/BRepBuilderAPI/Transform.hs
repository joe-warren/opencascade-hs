module OpenCascade.BRepBuilderAPI.Transform
( transform
) where

import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.C
import Foreign.Ptr
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import Data.Acquire
import OpenCascade.Internal.Bool (boolToCBool)

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepBuilderAPIContext)

C.include "<BRepBuilderAPI_Transform.hxx>"

-- needed to avoid a linker error about duplicate symbols
C.verbatim "extern template class NCollection_Map<TopoDS_Shape, TopTools_ShapeMapHasher>;"

transform :: Ptr TopoDS.Shape -> Ptr GP.Trsf -> Bool -> Acquire (Ptr TopoDS.Shape)
transform shape trsf copy =
  let cCopy = boolToCBool copy
      createTransform = [C.throwBlock| TopoDS_Shape* {
        BRepBuilderAPI_Transform transformer(*$(TopoDS_Shape* shape), *$(gp_Trsf* trsf), $(bool cCopy));
        return new TopoDS_Shape(transformer.Shape());
      } |]
  in mkAcquire createTransform TopoDS.Destructors.deleteShape
