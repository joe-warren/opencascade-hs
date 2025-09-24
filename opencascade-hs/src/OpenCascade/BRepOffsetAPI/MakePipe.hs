module OpenCascade.BRepOffsetAPI.MakePipe 
( MakePipe
, fromWireAndShape
, fromWireShapeTrihedronModeAndForceC1
) where

import OpenCascade.BRepOffsetAPI.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.BRepOffsetAPI.Types (MakePipe)
import OpenCascade.BRepOffsetAPI.Internal.Destructors (deleteMakePipe)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GeomFill as GeomFill
import Foreign.Ptr
import Foreign.C (CBool (..), CInt (..))
import OpenCascade.Internal.Bool (boolToCBool)
import Data.Acquire
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> brepOffsetAPIContext)

C.include "<BRepOffsetAPI_MakePipe.hxx>"
C.include "<TopoDS_Wire.hxx>"
C.include "<TopoDS_Shape.hxx>"
C.include "<GeomFill_Trihedron.hxx>"

fromWireAndShape :: Ptr TopoDS.Wire -> Ptr TopoDS.Shape -> Acquire (Ptr MakePipe)
fromWireAndShape wire profile =
  let createPipe = [C.throwBlock| BRepOffsetAPI_MakePipe* {
        return new BRepOffsetAPI_MakePipe(*$(TopoDS_Wire* wire), *$(TopoDS_Shape* profile));
      } |]
  in mkAcquire createPipe deleteMakePipe

fromWireShapeTrihedronModeAndForceC1 :: Ptr TopoDS.Wire -> Ptr TopoDS.Shape -> GeomFill.Trihedron -> Bool -> Acquire (Ptr MakePipe)
fromWireShapeTrihedronModeAndForceC1 wire profile trihedronMode forceC1 =
  let cTrihedronMode = fromIntegral $ fromEnum trihedronMode
      cForceC1 = boolToCBool forceC1
      createPipe = [C.throwBlock| BRepOffsetAPI_MakePipe* {
        return new BRepOffsetAPI_MakePipe(
          *$(TopoDS_Wire* wire), 
          *$(TopoDS_Shape* profile), 
          static_cast<GeomFill_Trihedron>($(int cTrihedronMode)), 
          $(bool cForceC1)
        );
      } |]
  in mkAcquire createPipe deleteMakePipe