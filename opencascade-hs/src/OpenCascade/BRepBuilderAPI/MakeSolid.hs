module OpenCascade.BRepBuilderAPI.MakeSolid
( MakeSolid
, new
, add
, solid
) where

import OpenCascade.BRepBuilderAPI.Types
import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.BRepBuilderAPI.Internal.Destructors
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Inheritance (upcast)
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepBuilderAPIContext)

C.include "<BRepBuilderAPI_MakeSolid.hxx>"
C.include "<TopoDS_Solid.hxx>" 

-- new

new :: Acquire (Ptr MakeSolid)
new = mkAcquire createMakeSolid deleteMakeSolid
  where
    createMakeSolid = [C.throwBlock| BRepBuilderAPI_MakeSolid* {
      return new BRepBuilderAPI_MakeSolid();
    } |]

-- add

add :: Ptr MakeSolid -> Ptr TopoDS.Shell -> IO ()
add makeSolid shell = [C.throwBlock| void {
  $(BRepBuilderAPI_MakeSolid* makeSolid)->Add(*$(TopoDS_Shell* shell));
} |]

-- solid

solid :: Ptr MakeSolid -> Acquire (Ptr TopoDS.Solid)
solid makeSolid = mkAcquire createSolid (deleteShape . upcast)
  where
    createSolid = [C.throwBlock| TopoDS_Solid* {
      return new TopoDS_Solid($(BRepBuilderAPI_MakeSolid* makeSolid)->Solid());
    } |]

