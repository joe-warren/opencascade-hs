module OpenCascade.BRepOffsetAPI.MakeOffsetShape 
( MakeOffsetShape 
, new
, performBySimple
, performByJoin
) where

import OpenCascade.BRepOffsetAPI.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.BRepOffsetAPI.Types (MakeOffsetShape)
import Data.Acquire
import OpenCascade.BRepOffsetAPI.Internal.Destructors (deleteMakeOffsetShape)
import Foreign (Ptr)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.C
import qualified OpenCascade.BRepOffset.Mode as BRepOffset.Mode
import qualified OpenCascade.GeomAbs.JoinType as GeomAbs.JoinType
import OpenCascade.Internal.Bool (boolToCBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> brepOffsetAPIContext)

C.include "<BRepOffsetAPI_MakeOffsetShape.hxx>"
C.include "<TopoDS_Shape.hxx>"
C.include "<BRepOffset_Mode.hxx>"
C.include "<GeomAbs_JoinType.hxx>"

new :: Acquire (Ptr MakeOffsetShape)
new =
  let createOffsetShape = [C.throwBlock| BRepOffsetAPI_MakeOffsetShape* {
        return new BRepOffsetAPI_MakeOffsetShape();
      } |]
  in mkAcquire createOffsetShape deleteMakeOffsetShape

performBySimple :: Ptr MakeOffsetShape -> Ptr TopoDS.Shape -> Double -> IO ()
performBySimple offsetShape shape value = do
  let cValue = realToFrac value
  [C.throwBlock| void {
    $(BRepOffsetAPI_MakeOffsetShape* offsetShape)->PerformBySimple(*$(TopoDS_Shape* shape), $(double cValue));
  } |]

-- |  builder, shape, value, tol, mode intersection, selfInter, join, removeIntEdges
performByJoin :: Ptr MakeOffsetShape -> Ptr TopoDS.Shape -> Double -> Double -> BRepOffset.Mode.Mode -> Bool -> Bool -> GeomAbs.JoinType.JoinType -> Bool -> IO ()
performByJoin offsetShape shape value tol mode intersection selfInter join removeIntEdges = do
  let cValue = realToFrac value
      cTol = realToFrac tol
      cMode = fromIntegral $ fromEnum mode
      cIntersection = boolToCBool intersection
      cSelfInter = boolToCBool selfInter
      cJoin = fromIntegral $ fromEnum join
      cRemoveIntEdges = boolToCBool removeIntEdges
  [C.throwBlock| void {
    $(BRepOffsetAPI_MakeOffsetShape* offsetShape)->PerformByJoin(
      *$(TopoDS_Shape* shape), 
      $(double cValue), 
      $(double cTol), 
      static_cast<BRepOffset_Mode>($(int cMode)), 
      $(bool cIntersection), 
      $(bool cSelfInter), 
      static_cast<GeomAbs_JoinType>($(int cJoin)), 
      $(bool cRemoveIntEdges)
    );
  } |]

