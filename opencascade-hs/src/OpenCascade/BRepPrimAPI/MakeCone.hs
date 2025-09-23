module OpenCascade.BRepPrimAPI.MakeCone
( fromTwoRadiiAndHeight
, solid
) where

import OpenCascade.BRepPrimAPI.Types (MakeCone)
import OpenCascade.BRepPrimAPI.Internal.Context
import OpenCascade.BRepPrimAPI.Internal.Destructors (deleteMakeCone)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire 

C.context (C.cppCtx <> brepPrimAPIContext)

C.include "<BRepPrimAPI_MakeCone.hxx>"

fromTwoRadiiAndHeight :: Double -> Double -> Double -> Acquire (Ptr MakeCone)
fromTwoRadiiAndHeight radius1 radius2 height = mkAcquire createMakeCone deleteMakeCone
  where
    createMakeCone = 
      let cRadius1 = realToFrac radius1
          cRadius2 = realToFrac radius2
          cHeight = realToFrac height
      in [C.throwBlock| BRepPrimAPI_MakeCone* {
        return new BRepPrimAPI_MakeCone($(double cRadius1), $(double cRadius2), $(double cHeight));
      } |]

solid :: Ptr MakeCone -> Acquire (Ptr TopoDS.Solid)
solid makeCone = mkAcquire createSolid (TopoDS.Destructors.deleteShape . castPtr)
  where
    createSolid = [C.throwBlock| TopoDS_Solid* {
      return new TopoDS_Solid($(BRepPrimAPI_MakeCone* makeCone)->Solid());
    } |]