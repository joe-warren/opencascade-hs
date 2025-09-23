module OpenCascade.BRepBuilderAPI.MakePolygon
( from3Pnts 
) where

import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Inheritance (upcast)
import OpenCascade.Internal.Bool (boolToCBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.C (CBool (..))
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepBuilderAPIContext)

C.include "<BRepBuilderAPI_MakePolygon.hxx>"
C.include "<TopoDS_Wire.hxx>"

from3Pnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> Ptr GP.Pnt -> Bool -> Acquire (Ptr TopoDS.Wire)
from3Pnts p1 p2 p3 close = mkAcquire createPolygon (deleteShape . upcast)
  where
    createPolygon = 
      let cClose = boolToCBool close
      in [C.throwBlock| TopoDS_Wire* {
        BRepBuilderAPI_MakePolygon maker(*$(gp_Pnt* p1), *$(gp_Pnt* p2), *$(gp_Pnt* p3), $(bool cClose));
        return new TopoDS_Wire(maker.Wire());
      } |]