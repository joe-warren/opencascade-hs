module OpenCascade.TopoDS.Builder 
( Builder 
, new
, makeWire
, makeShell
, makeSolid
, makeCompSolid
, makeCompound
, add
, remove
) where

import OpenCascade.TopoDS.Types 
import OpenCascade.TopoDS.Internal.Context
import OpenCascade.TopoDS.Internal.Destructors (deleteBuilder)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)

C.context (C.cppCtx <> topoDSContext)

C.include "<TopoDS_Builder.hxx>"

new :: Acquire (Ptr Builder)
new = mkAcquire createBuilder deleteBuilder
  where
    createBuilder = [C.throwBlock| TopoDS_Builder* {
      return new TopoDS_Builder();
    } |]

makeWire :: Ptr Builder -> Ptr Wire -> IO ()
makeWire builder wire = [C.throwBlock| void {
  $(TopoDS_Builder* builder)->MakeWire(*$(TopoDS_Wire* wire));
} |]

makeShell :: Ptr Builder -> Ptr Shell -> IO ()
makeShell builder shell = [C.throwBlock| void {
  $(TopoDS_Builder* builder)->MakeShell(*$(TopoDS_Shell* shell));
} |]

makeSolid :: Ptr Builder -> Ptr Solid -> IO ()
makeSolid builder solid = [C.throwBlock| void {
  $(TopoDS_Builder* builder)->MakeSolid(*$(TopoDS_Solid* solid));
} |]

makeCompSolid :: Ptr Builder -> Ptr CompSolid -> IO ()
makeCompSolid builder compSolid = [C.throwBlock| void {
  $(TopoDS_Builder* builder)->MakeCompSolid(*$(TopoDS_CompSolid* compSolid));
} |]

makeCompound :: Ptr Builder -> Ptr Compound -> IO ()
makeCompound builder compound = [C.throwBlock| void {
  $(TopoDS_Builder* builder)->MakeCompound(*$(TopoDS_Compound* compound));
} |]

add :: Ptr Builder -> Ptr Shape -> Ptr Shape -> IO ()
add builder containerShape shapeToAdd = [C.throwBlock| void {
  $(TopoDS_Builder* builder)->Add(*$(TopoDS_Shape* containerShape), *$(TopoDS_Shape* shapeToAdd));
} |]

remove :: Ptr Builder -> Ptr Shape -> Ptr Shape -> IO ()
remove builder containerShape shapeToRemove = [C.throwBlock| void {
  $(TopoDS_Builder* builder)->Remove(*$(TopoDS_Shape* containerShape), *$(TopoDS_Shape* shapeToRemove));
} |]
