module OpenCascade.BRepFilletAPI.MakeFillet 
( MakeFillet
, fromShape
, addEdge
, addEdgeWithRadius
, addEdgeWithTwoRadiuses
, reset
, nbFaultyContours
, faultyContour
, nbEdges
, edge
, remove
) where

import OpenCascade.BRepFilletAPI.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.BRepFilletAPI.Types (MakeFillet)
import OpenCascade.BRepFilletAPI.Internal.Destructors (deleteMakeFillet)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Inheritance (upcast)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Foreign.C
import Data.Acquire

C.context (C.cppCtx <> topoDSContext <> brepFilletAPIContext)

C.include "<BRepFilletAPI_MakeFillet.hxx>"
C.include "<TopoDS_Shape.hxx>"
C.include "<TopoDS_Edge.hxx>"


fromShape :: Ptr TopoDS.Shape -> Acquire (Ptr MakeFillet)
fromShape shape =
  let createFillet = [C.throwBlock| BRepFilletAPI_MakeFillet* {
        return new BRepFilletAPI_MakeFillet(*$(TopoDS_Shape* shape));
      } |]
  in mkAcquire createFillet deleteMakeFillet

addEdge :: Ptr MakeFillet -> Ptr TopoDS.Edge -> IO ()
addEdge fillet edge = [C.throwBlock| void {
  $(BRepFilletAPI_MakeFillet* fillet)->Add(*$(TopoDS_Edge* edge));
} |]

addEdgeWithRadius :: Ptr MakeFillet -> Double -> Ptr TopoDS.Edge -> IO ()
addEdgeWithRadius fillet radius edge = do
  let cRadius = realToFrac radius
  [C.throwBlock| void {
    $(BRepFilletAPI_MakeFillet* fillet)->Add($(double cRadius), *$(TopoDS_Edge* edge));
  } |]

addEdgeWithTwoRadiuses :: Ptr MakeFillet -> Double -> Double -> Ptr TopoDS.Edge -> IO ()
addEdgeWithTwoRadiuses fillet radius1 radius2 edge = do
  let cRadius1 = realToFrac radius1
      cRadius2 = realToFrac radius2
  [C.throwBlock| void {
    $(BRepFilletAPI_MakeFillet* fillet)->Add($(double cRadius1), $(double cRadius2), *$(TopoDS_Edge* edge));
  } |]

reset :: Ptr MakeFillet -> IO ()
reset fillet = [C.throwBlock| void {
  $(BRepFilletAPI_MakeFillet* fillet)->Reset();
} |]

nbFaultyContours :: Ptr MakeFillet -> IO Int
nbFaultyContours fillet = do
  result <- [C.throwBlock| int {
    return $(BRepFilletAPI_MakeFillet* fillet)->NbFaultyContours();
  } |]
  return (fromIntegral result)

faultyContour :: Ptr MakeFillet -> Int -> IO Int
faultyContour fillet index = do
  let cIndex = fromIntegral index
  result <- [C.throwBlock| int {
    return $(BRepFilletAPI_MakeFillet* fillet)->FaultyContour($(int cIndex));
  } |]
  return (fromIntegral result)

nbEdges :: Ptr MakeFillet -> Int -> IO Int
nbEdges fillet contourIndex = do
  let cContourIndex = fromIntegral contourIndex
  result <- [C.throwBlock| int {
    return $(BRepFilletAPI_MakeFillet* fillet)->NbEdges($(int cContourIndex));
  } |]
  return (fromIntegral result)

edge :: Ptr MakeFillet -> Int -> Int -> Acquire (Ptr TopoDS.Edge)
edge fillet contourIndex edgeIndex =
  let cContourIndex = fromIntegral contourIndex
      cEdgeIndex = fromIntegral edgeIndex
      createEdge = [C.throwBlock| TopoDS_Edge* {
        return new TopoDS_Edge($(BRepFilletAPI_MakeFillet* fillet)->Edge($(int cContourIndex), $(int cEdgeIndex)));
      } |]
  in mkAcquire createEdge (deleteShape . upcast)

remove :: Ptr MakeFillet -> Ptr TopoDS.Edge -> IO ()
remove fillet edge = [C.throwBlock| void {
  $(BRepFilletAPI_MakeFillet* fillet)->Remove(*$(TopoDS_Edge* edge));
} |]