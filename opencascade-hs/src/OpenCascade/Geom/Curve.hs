module OpenCascade.Geom.Curve 
( value
, firstParameter
, lastParameter
, dn
, reversedParameter
, reversed
) where

import OpenCascade.Geom.Internal.Context
import OpenCascade.Handle.Internal.Context (handleContext)
import OpenCascade.GP.Internal.Context (gpContext)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Foreign.C
import Data.Acquire
import OpenCascade.Geom.Types (Curve)
import OpenCascade.GP (Pnt, Vec)
import OpenCascade.GP.Internal.Destructors (deletePnt, deleteVec)
import OpenCascade.Geom.Internal.Destructors (deleteHandleCurve)
import OpenCascade.Handle (Handle)

C.context (C.cppCtx <> gpContext <> handleContext <> geomContext)

C.include "<Geom_Curve.hxx>"
C.include "<Standard_Handle.hxx>"
C.include "<gp_Pnt.hxx>"
C.include "<gp_Vec.hxx>"

value :: Ptr (Handle Curve) -> Double -> Acquire (Ptr Pnt)
value curve u =
  let cU = realToFrac u
      createPoint = [C.throwBlock| gp_Pnt* {
        return new gp_Pnt((*$(opencascade::handle<Geom_Curve>* curve))->Value($(double cU)));
      } |]
  in mkAcquire createPoint deletePnt

firstParameter :: Ptr (Handle Curve) -> IO Double
firstParameter curve = do
  result <- [C.throwBlock| double {
    return (*$(opencascade::handle<Geom_Curve>* curve))->FirstParameter();
  } |]
  return (realToFrac result)

lastParameter :: Ptr (Handle Curve) -> IO Double
lastParameter curve = do
  result <- [C.throwBlock| double {
    return (*$(opencascade::handle<Geom_Curve>* curve))->LastParameter();
  } |]
  return (realToFrac result)

dn :: Ptr (Handle Curve) -> Double -> Int -> Acquire (Ptr Vec)
dn curve u n =
  let cU = realToFrac u
      cN = fromIntegral n
      createVec = [C.throwBlock| gp_Vec* {
        return new gp_Vec((*$(opencascade::handle<Geom_Curve>* curve))->DN($(double cU), $(int cN)));
      } |]
  in mkAcquire createVec deleteVec

reversedParameter :: Ptr (Handle Curve) -> Double -> IO Double
reversedParameter curve u = do
  let cU = realToFrac u
  result <- [C.throwBlock| double {
    return (*$(opencascade::handle<Geom_Curve>* curve))->ReversedParameter($(double cU));
  } |]
  return (realToFrac result)

reversed :: Ptr (Handle Curve) -> Acquire (Ptr (Handle Curve))
reversed curve =
  let createReversed = [C.throwBlock| opencascade::handle<Geom_Curve>* {
    return new opencascade::handle<Geom_Curve>((*$(opencascade::handle<Geom_Curve>* curve))->Reversed());
  } |]
  in mkAcquire createReversed deleteHandleCurve
