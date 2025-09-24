module OpenCascade.Geom.BSplineCurve 
( toHandle
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
import OpenCascade.Geom.Types (BSplineCurve)
import OpenCascade.Geom.Internal.Destructors (deleteHandleBSplineCurve)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.GP (Pnt)
import OpenCascade.GP.Internal.Destructors (deletePnt)

C.context (C.cppCtx <> gpContext <> handleContext <> geomContext)

C.include "<Geom_BSplineCurve.hxx>"
C.include "<Standard_Handle.hxx>"
C.include "<gp_Pnt.hxx>"

toHandle :: Ptr BSplineCurve -> Acquire (Ptr (Handle BSplineCurve))
toHandle curve =
  let createHandle = [C.throwBlock| opencascade::handle<Geom_BSplineCurve>* {
        return new opencascade::handle<Geom_BSplineCurve>($(Geom_BSplineCurve* curve));
      } |]
  in mkAcquire createHandle deleteHandleBSplineCurve

nbPoles :: Ptr (Handle BSplineCurve) -> IO Int 
nbPoles handle = do
  result <- [C.throwBlock| int {
    return (*$(opencascade::handle<Geom_BSplineCurve>* handle))->NbPoles();
  } |]
  return (fromIntegral result)

pole :: Ptr (Handle BSplineCurve) -> Int -> Acquire (Ptr Pnt)
pole handle n =
  let cN = fromIntegral n
      createPole = [C.throwBlock| gp_Pnt* {
        return new gp_Pnt((*$(opencascade::handle<Geom_BSplineCurve>* handle))->Pole($(int cN)));
      } |]
  in mkAcquire createPole deletePnt

isRational :: Ptr (Handle BSplineCurve) -> IO Bool
isRational handle = do
  result <- [C.throwBlock| bool {
    return (*$(opencascade::handle<Geom_BSplineCurve>* handle))->IsRational();
  } |]
  return (cBoolToBool result)

segment :: Ptr (Handle BSplineCurve) -> Double -> Double -> Double -> IO ()
segment handle u1 u2 tolerance = do
  let cU1 = realToFrac u1
      cU2 = realToFrac u2
      cTolerance = realToFrac tolerance
  [C.throwBlock| void {
    (*$(opencascade::handle<Geom_BSplineCurve>* handle))->Segment($(double cU1), $(double cU2), $(double cTolerance));
  } |]

