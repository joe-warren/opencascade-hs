module OpenCascade.BRepBuilderAPI.MakeWire 
( MakeWire
, new 
, addEdge
, addWire
, addListOfShape
, wire
, vertex
, isDone
, error
) where

import Prelude hiding (error)
import OpenCascade.BRepBuilderAPI.Types
import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.BRepBuilderAPI.Internal.Destructors
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.TopTools.Internal.Context (topToolsContext)
import OpenCascade.Inheritance
import OpenCascade.Internal.Bool
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified OpenCascade.TopTools as TopTools
import OpenCascade.BRepBuilderAPI.WireError (WireError)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext <> topoDSContext <> topToolsContext <> brepBuilderAPIContext)

C.include "<BRepBuilderAPI_MakeWire.hxx>"
C.include "<TopoDS_Wire.hxx>"
C.include "<TopoDS_Vertex.hxx>" 

-- new

new :: Acquire (Ptr MakeWire)
new = mkAcquire createMakeWire deleteMakeWire
  where
    createMakeWire = [C.throwBlock| BRepBuilderAPI_MakeWire* {
      return new BRepBuilderAPI_MakeWire();
    } |]

-- addEdge 

addEdge :: Ptr MakeWire -> Ptr TopoDS.Edge -> IO ()
addEdge makeWire edge = [C.throwBlock| void {
  $(BRepBuilderAPI_MakeWire* makeWire)->Add(*$(TopoDS_Edge* edge));
} |]

-- addWire 

addWire :: Ptr MakeWire -> Ptr TopoDS.Wire -> IO ()
addWire makeWire wireToAdd = [C.throwBlock| void {
  $(BRepBuilderAPI_MakeWire* makeWire)->Add(*$(TopoDS_Wire* wireToAdd));
} |]

-- addListOfShape

addListOfShape :: Ptr MakeWire -> Ptr TopTools.ListOfShape -> IO ()
addListOfShape makeWire listOfShape = [C.throwBlock| void {
  $(BRepBuilderAPI_MakeWire* makeWire)->Add(*$(TopTools_ListOfShape* listOfShape));
} |]

-- wire

wire :: Ptr MakeWire -> Acquire (Ptr TopoDS.Wire)
wire makeWire = mkAcquire createWire (TopoDS.Destructors.deleteShape . upcast)
  where
    createWire = [C.throwBlock| TopoDS_Wire* {
      return new TopoDS_Wire($(BRepBuilderAPI_MakeWire* makeWire)->Wire());
    } |]

-- vertex

vertex :: Ptr MakeWire -> Acquire (Ptr TopoDS.Vertex)
vertex makeWire = mkAcquire createVertex (TopoDS.Destructors.deleteShape . upcast)
  where
    createVertex = [C.throwBlock| TopoDS_Vertex* {
      return new TopoDS_Vertex($(BRepBuilderAPI_MakeWire* makeWire)->Vertex());
    } |]

-- isDone

isDone :: Ptr MakeWire -> IO Bool
isDone makeWire = do
  result <- [C.throwBlock| bool {
    return $(BRepBuilderAPI_MakeWire* makeWire)->IsDone();
  } |]
  return (cBoolToBool result)

-- error

error :: Ptr MakeWire -> IO WireError 
error makeWire = do
  result <- [C.throwBlock| int {
    return static_cast<int>($(BRepBuilderAPI_MakeWire* makeWire)->Error());
  } |]
  return (toEnum . fromIntegral $ result)
