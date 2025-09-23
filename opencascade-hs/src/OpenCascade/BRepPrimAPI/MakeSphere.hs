module OpenCascade.BRepPrimAPI.MakeSphere
( fromRadius
, fromPntAndRadius
, solid
) where

import OpenCascade.BRepPrimAPI.Types (MakeSphere)
import OpenCascade.BRepPrimAPI.Internal.Context
import OpenCascade.BRepPrimAPI.Internal.Destructors (deleteMakeSphere)
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire 

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepPrimAPIContext)

C.include "<BRepPrimAPI_MakeSphere.hxx>"

fromRadius :: Double -> Acquire (Ptr MakeSphere)
fromRadius radius = mkAcquire createMakeSphere deleteMakeSphere
  where
    createMakeSphere = 
      let cRadius = realToFrac radius
      in [C.throwBlock| BRepPrimAPI_MakeSphere* {
        return new BRepPrimAPI_MakeSphere($(double cRadius));
      } |]

fromPntAndRadius :: Ptr GP.Pnt -> Double -> Acquire (Ptr MakeSphere)
fromPntAndRadius center radius = mkAcquire createMakeSphere deleteMakeSphere
  where
    createMakeSphere = 
      let cRadius = realToFrac radius
      in [C.throwBlock| BRepPrimAPI_MakeSphere* {
        return new BRepPrimAPI_MakeSphere(*$(gp_Pnt* center), $(double cRadius));
      } |]

solid :: Ptr MakeSphere -> Acquire (Ptr TopoDS.Solid)
solid makeSphere = mkAcquire createSolid (TopoDS.Destructors.deleteShape . castPtr)
  where
    createSolid = [C.throwBlock| TopoDS_Solid* {
      return new TopoDS_Solid($(BRepPrimAPI_MakeSphere* makeSphere)->Solid());
    } |]
