module OpenCascade.BRepPrimAPI.MakePrism
( fromVec
, fromDir
, shape
) where

import OpenCascade.BRepPrimAPI.Types (MakePrism)
import OpenCascade.BRepPrimAPI.Internal.Context
import OpenCascade.BRepPrimAPI.Internal.Destructors (deleteMakePrism)
import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Bool
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire 

C.context (C.cppCtx <> brepPrimAPIContext)

C.include "<BRepPrimAPI_MakePrism.hxx>"

fromVec :: Ptr TopoDS.Shape -> Ptr GP.Vec -> Bool -> Bool -> Acquire (Ptr MakePrism)
fromVec inputShape vec copy canonize = mkAcquire createMakePrism deleteMakePrism
  where
    createMakePrism = 
      let cCopy = boolToCBool copy
          cCanonize = boolToCBool canonize
      in [C.throwBlock| BRepPrimAPI_MakePrism* {
        return new BRepPrimAPI_MakePrism(*$(TopoDS_Shape* inputShape), *$(gp_Vec* vec), $(bool cCopy), $(bool cCanonize));
      } |]

fromDir :: Ptr TopoDS.Shape -> Ptr GP.Dir -> Bool -> Bool -> Bool -> Acquire (Ptr MakePrism)
fromDir inputShape dir inf copy canonize = mkAcquire createMakePrism deleteMakePrism
  where
    createMakePrism = 
      let cInf = boolToCBool inf
          cCopy = boolToCBool copy
          cCanonize = boolToCBool canonize
      in [C.throwBlock| BRepPrimAPI_MakePrism* {
        return new BRepPrimAPI_MakePrism(*$(TopoDS_Shape* inputShape), *$(gp_Dir* dir), $(bool cInf), $(bool cCopy), $(bool cCanonize));
      } |]

shape :: Ptr MakePrism -> Acquire (Ptr TopoDS.Shape)
shape makePrism = mkAcquire createShape TopoDS.Destructors.deleteShape
  where
    createShape = [C.throwBlock| TopoDS_Shape* {
      return new TopoDS_Shape($(BRepPrimAPI_MakePrism* makePrism)->Shape());
    } |]
