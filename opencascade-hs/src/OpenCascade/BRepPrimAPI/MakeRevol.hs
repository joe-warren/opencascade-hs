module OpenCascade.BRepPrimAPI.MakeRevol 
( MakeRevol
, fromShapeAndAx1
, shape
) where

import OpenCascade.BRepPrimAPI.Types (MakeRevol)
import OpenCascade.BRepPrimAPI.Internal.Context
import OpenCascade.BRepPrimAPI.Internal.Destructors (deleteMakeRevol)
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified OpenCascade.GP as GP
import OpenCascade.Internal.Bool (boolToCBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepPrimAPIContext)

C.include "<BRepPrimAPI_MakeRevol.hxx>"
C.include "<TopoDS_Shape.hxx>"

fromShapeAndAx1 :: Ptr TopoDS.Shape -> Ptr GP.Ax1 -> Bool -> Acquire (Ptr MakeRevol)
fromShapeAndAx1 inputShape axis copy = mkAcquire createMakeRevol deleteMakeRevol
  where
    createMakeRevol = 
      let cCopy = boolToCBool copy
      in [C.throwBlock| BRepPrimAPI_MakeRevol* {
        return new BRepPrimAPI_MakeRevol(*$(TopoDS_Shape* inputShape), *$(gp_Ax1* axis), $(bool cCopy));
      } |]

shape :: Ptr MakeRevol -> Acquire (Ptr TopoDS.Shape)
shape makeRevol = mkAcquire createShape TopoDS.Destructors.deleteShape
  where
    createShape = [C.throwBlock| TopoDS_Shape* {
      return new TopoDS_Shape($(BRepPrimAPI_MakeRevol* makeRevol)->Shape());
    } |]
