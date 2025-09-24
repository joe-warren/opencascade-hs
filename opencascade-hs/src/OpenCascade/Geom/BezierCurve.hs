module OpenCascade.Geom.BezierCurve 
( fromPnts
, toHandle
, nbPoles
, pole
, isRational
, segment
) where

import OpenCascade.Geom.Internal.Context
import OpenCascade.Handle.Internal.Context (handleContext)
import OpenCascade.GP.Internal.Context (gpContext)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Foreign.C (CInt (..), CBool (..), CDouble (..))
import Data.Acquire
import OpenCascade.Geom.Types (BezierCurve)
import OpenCascade.Geom.Internal.Destructors (deleteBezierCurve, deleteHandleBezierCurve)
import OpenCascade.GP.Internal.Destructors (deletePnt)
import OpenCascade.GP (Pnt)
import OpenCascade.NCollection (Array1)
import OpenCascade.NCollection.Internal.Context (nCollectionContext)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Bool (cBoolToBool)

C.context (C.cppCtx <> gpContext <> handleContext <> nCollectionContext <> geomContext)

C.include "<Geom_BezierCurve.hxx>"
C.include "<Standard_Handle.hxx>"
C.include "<gp_Pnt.hxx>"
C.include "<NCollection_Array1.hxx>"

fromPnts :: Ptr (Array1 Pnt) -> Acquire (Ptr BezierCurve)
fromPnts arr =
  let createCurve = [C.throwBlock| Geom_BezierCurve* {
        return new Geom_BezierCurve(*$(NCollection_Array1<gp_Pnt>* arr));
      } |]
  in mkAcquire createCurve deleteBezierCurve

toHandle :: Ptr BezierCurve -> Acquire (Ptr (Handle BezierCurve))
toHandle curve =
  let createHandle = [C.throwBlock| opencascade::handle<Geom_BezierCurve>* {
        return new opencascade::handle<Geom_BezierCurve>($(Geom_BezierCurve* curve));
      } |]
  in mkAcquire createHandle deleteHandleBezierCurve

nbPoles :: Ptr (Handle BezierCurve) -> IO Int 
nbPoles handle = do
  result <- [C.throwBlock| int {
    return (*$(opencascade::handle<Geom_BezierCurve>* handle))->NbPoles();
  } |]
  return (fromIntegral result)

pole :: Ptr (Handle BezierCurve) -> Int -> Acquire (Ptr Pnt)
pole handle n =
  let cN = fromIntegral n
      createPole = [C.throwBlock| gp_Pnt* {
        return new gp_Pnt((*$(opencascade::handle<Geom_BezierCurve>* handle))->Pole($(int cN)));
      } |]
  in mkAcquire createPole deletePnt

isRational :: Ptr (Handle BezierCurve) -> IO Bool
isRational handle = do
  result <- [C.throwBlock| bool {
    return (*$(opencascade::handle<Geom_BezierCurve>* handle))->IsRational();
  } |]
  return (cBoolToBool result)

segment :: Ptr (Handle BezierCurve) -> Double -> Double -> IO ()
segment handle u1 u2 = do
  let cU1 = realToFrac u1
      cU2 = realToFrac u2
  [C.throwBlock| void {
    (*$(opencascade::handle<Geom_BezierCurve>* handle))->Segment($(double cU1), $(double cU2));
  } |]