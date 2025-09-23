module OpenCascade.BRepPrimAPI.MakeBox
( fromPnts
, solid
, shell
) where

import OpenCascade.BRepPrimAPI.Types (MakeBox)
import OpenCascade.BRepPrimAPI.Internal.Context
import OpenCascade.BRepPrimAPI.Internal.Destructors (deleteMakeBox)
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import qualified OpenCascade.GP as GP
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepPrimAPIContext)

C.include "<BRepPrimAPI_MakeBox.hxx>"

-- fromPnts

fromPnts :: Ptr GP.Pnt -> Ptr GP.Pnt -> Acquire (Ptr MakeBox)
fromPnts pnt1 pnt2 = mkAcquire createMakeBox deleteMakeBox
  where
    createMakeBox = [C.throwBlock| BRepPrimAPI_MakeBox* {
      return new BRepPrimAPI_MakeBox(*$(gp_Pnt* pnt1), *$(gp_Pnt* pnt2));
    } |]

-- solid

solid :: Ptr MakeBox -> Acquire (Ptr TopoDS.Solid)
solid makeBox = mkAcquire createSolid (deleteShape . castPtr)
  where
    createSolid = [C.throwBlock| TopoDS_Solid* {
      return new TopoDS_Solid($(BRepPrimAPI_MakeBox* makeBox)->Solid());
    } |]

-- shell

shell :: Ptr MakeBox -> Acquire (Ptr TopoDS.Shell)
shell makeBox = mkAcquire createShell (deleteShape . castPtr)
  where
    createShell = [C.throwBlock| TopoDS_Shell* {
      return new TopoDS_Shell($(BRepPrimAPI_MakeBox* makeBox)->Shell());
    } |]



