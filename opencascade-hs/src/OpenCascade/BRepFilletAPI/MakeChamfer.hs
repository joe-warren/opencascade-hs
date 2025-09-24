module OpenCascade.BRepFilletAPI.MakeChamfer 
( MakeChamfer
, fromShape
, addEdge
, addEdgeWithDistance
, reset
, nbEdges
, edge
, remove
) where

import OpenCascade.BRepFilletAPI.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.BRepFilletAPI.Types (MakeChamfer)
import OpenCascade.BRepFilletAPI.Internal.Destructors (deleteMakeChamfer)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Inheritance (upcast)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Foreign.C
import Data.Acquire

C.context (C.cppCtx <> topoDSContext <> brepFilletAPIContext)

C.include "<BRepFilletAPI_MakeChamfer.hxx>"
C.include "<TopoDS_Shape.hxx>"
C.include "<TopoDS_Edge.hxx>"


fromShape :: Ptr TopoDS.Shape -> Acquire (Ptr MakeChamfer)
fromShape shape =
  let createChamfer = [C.throwBlock| BRepFilletAPI_MakeChamfer* {
        return new BRepFilletAPI_MakeChamfer(*$(TopoDS_Shape* shape));
      } |]
  in mkAcquire createChamfer deleteMakeChamfer

addEdge :: Ptr MakeChamfer -> Ptr TopoDS.Edge -> IO ()
addEdge chamfer edge = [C.throwBlock| void {
  $(BRepFilletAPI_MakeChamfer* chamfer)->Add(*$(TopoDS_Edge* edge));
} |]

addEdgeWithDistance :: Ptr MakeChamfer -> Double -> Ptr TopoDS.Edge -> IO ()
addEdgeWithDistance chamfer distance edge = do
  let cDistance = realToFrac distance
  [C.throwBlock| void {
    $(BRepFilletAPI_MakeChamfer* chamfer)->Add($(double cDistance), *$(TopoDS_Edge* edge));
  } |]

reset :: Ptr MakeChamfer -> IO ()
reset chamfer = [C.throwBlock| void {
  $(BRepFilletAPI_MakeChamfer* chamfer)->Reset();
} |]

nbEdges :: Ptr MakeChamfer -> Int -> IO Int
nbEdges chamfer contourIndex = do
  let cContourIndex = fromIntegral contourIndex
  result <- [C.throwBlock| int {
    return $(BRepFilletAPI_MakeChamfer* chamfer)->NbEdges($(int cContourIndex));
  } |]
  return (fromIntegral result)

edge :: Ptr MakeChamfer -> Int -> Int -> Acquire (Ptr TopoDS.Edge)
edge chamfer contourIndex edgeIndex =
  let cContourIndex = fromIntegral contourIndex
      cEdgeIndex = fromIntegral edgeIndex
      createEdge = [C.throwBlock| TopoDS_Edge* {
        return new TopoDS_Edge($(BRepFilletAPI_MakeChamfer* chamfer)->Edge($(int cContourIndex), $(int cEdgeIndex)));
      } |]
  in mkAcquire createEdge (deleteShape . upcast)

remove :: Ptr MakeChamfer -> Ptr TopoDS.Edge -> IO ()
remove chamfer edge = [C.throwBlock| void {
  $(BRepFilletAPI_MakeChamfer* chamfer)->Remove(*$(TopoDS_Edge* edge));
} |]