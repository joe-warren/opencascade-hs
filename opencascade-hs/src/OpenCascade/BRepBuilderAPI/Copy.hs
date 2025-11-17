module OpenCascade.BRepBuilderAPI.Copy
( copy
) where

import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Bool
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepBuilderAPIContext)

C.include "<TopTools_ListOfShape.hxx>"
C.include "<BRepBuilderAPI_Copy.hxx>"

copy :: Ptr TopoDS.Shape -> Bool -> Bool -> Acquire (Ptr TopoDS.Shape)
copy shape copyGeom copyMesh =
  let cCopyGeom = boolToCBool copyGeom
      cCopyMesh = boolToCBool copyMesh
      createCopy = [C.throwBlock| TopoDS_Shape* {
        BRepBuilderAPI_Copy copier(*$(TopoDS_Shape* shape), $(bool cCopyGeom), $(bool cCopyMesh));
        return new TopoDS_Shape(copier.Shape());
      } |]
  in mkAcquire createCopy TopoDS.Destructors.deleteShape