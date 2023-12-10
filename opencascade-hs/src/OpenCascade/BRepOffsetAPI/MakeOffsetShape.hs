{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepOffsetAPI.MakeOffsetShape 
( MakeOffsetShape 
, new
, performBySimple
, performByJoin
) where

import OpenCascade.BRepOffsetAPI.Types (MakeOffsetShape)
import Data.Acquire
import OpenCascade.BRepOffsetAPI.Internal.Destructors (deleteMakeOffsetShape)
import Foreign (Ptr)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.C
import Data.Coerce (coerce)
import qualified OpenCascade.BRepOffset.Mode as BRepOffset.Mode
import qualified OpenCascade.GeomAbs.JoinType as GeomAbs.JoinType
import OpenCascade.Internal.Bool (boolToCBool)

foreign import capi unsafe "hs_BRepOffsetAPI_MakeOffsetShape.h hs_new_BRepOffsetAPI_MakeOffsetShape" rawNew :: IO (Ptr MakeOffsetShape)

new :: Acquire (Ptr MakeOffsetShape)
new = mkAcquire rawNew deleteMakeOffsetShape

foreign import capi unsafe "hs_BRepOffsetAPI_MakeOffsetShape.h hs_BRepOffsetAPI_MakeOffsetShape_performBySimple" rawPerformBySimple :: Ptr MakeOffsetShape -> Ptr TopoDS.Shape -> CDouble -> IO ()
 
performBySimple :: Ptr MakeOffsetShape -> Ptr TopoDS.Shape -> Double -> IO ()
performBySimple = coerce rawPerformBySimple


foreign import capi unsafe "hs_BRepOffsetAPI_MakeOffsetShape.h hs_BRepOffsetAPI_MakeOffsetShape_performByJoin" rawPerformByJoin :: Ptr MakeOffsetShape -> Ptr TopoDS.Shape -> CDouble -> CDouble -> CInt -> CBool -> CBool -> CInt -> CBool -> IO ()

-- |  builder, shape, value, tol, mode intersection, selfInter, join, removeIntEdges
performByJoin :: Ptr MakeOffsetShape -> Ptr TopoDS.Shape -> Double -> Double -> BRepOffset.Mode.Mode -> Bool -> Bool -> GeomAbs.JoinType.JoinType -> Bool -> IO ()
performByJoin builder shape value tol mode intersection selfInter join removeIntEdges =
    rawPerformByJoin builder shape (coerce value) (coerce tol) (fromIntegral $ fromEnum mode) (boolToCBool intersection) (boolToCBool selfInter) (fromIntegral . fromEnum $ join) (boolToCBool removeIntEdges)

