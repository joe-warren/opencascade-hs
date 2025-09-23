module OpenCascade.BRepPrimAPI.MakeCylinder
( fromRadiusAndHeight
, solid
) where

import OpenCascade.BRepPrimAPI.Types (MakeCylinder)
import OpenCascade.BRepPrimAPI.Internal.Context
import OpenCascade.BRepPrimAPI.Internal.Destructors (deleteMakeCylinder)
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire 

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepPrimAPIContext)

C.include "<BRepPrimAPI_MakeCylinder.hxx>"

fromRadiusAndHeight :: Double -> Double -> Acquire (Ptr MakeCylinder)
fromRadiusAndHeight radius height = mkAcquire createMakeCylinder deleteMakeCylinder
  where
    createMakeCylinder = 
      let cRadius = realToFrac radius
          cHeight = realToFrac height
      in [C.throwBlock| BRepPrimAPI_MakeCylinder* {
        return new BRepPrimAPI_MakeCylinder($(double cRadius), $(double cHeight));
      } |]

solid :: Ptr MakeCylinder -> Acquire (Ptr TopoDS.Solid)
solid makeCylinder = mkAcquire createSolid (TopoDS.Destructors.deleteShape . castPtr)
  where
    createSolid = [C.throwBlock| TopoDS_Solid* {
      return new TopoDS_Solid($(BRepPrimAPI_MakeCylinder* makeCylinder)->Solid());
    } |]