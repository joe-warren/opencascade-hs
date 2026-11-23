{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeUpgrade.ShapeDivide
( ShapeDivide
, perform
, result
, getContext
, setPrecision
, setMaxTolerance
, setMinTolerance
) where

import OpenCascade.ShapeUpgrade.Types (ShapeDivide)
import qualified OpenCascade.ShapeBuild.Types as ShapeBuild
import OpenCascade.ShapeBuild.Internal.Destructors (deleteHandleReShape)
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Bool (boolToCBool, cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import Data.Acquire (Acquire, mkAcquire)
import Data.Coerce (coerce)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CBool (..), CDouble (..))

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivide.h hs_ShapeUpgrade_ShapeDivide_perform" rawPerform
    :: Ptr ShapeDivide
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CBool

perform :: Ptr ShapeDivide -> Bool -> IO Bool
perform divide newContext = cBoolToBool <$> wrapException (rawPerform divide (boolToCBool newContext))

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivide.h hs_ShapeUpgrade_ShapeDivide_result" rawResult
    :: Ptr ShapeDivide
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

result :: Ptr ShapeDivide -> Acquire (Ptr TopoDS.Shape)
result divide = mkAcquire (wrapException $ rawResult divide) deleteShape

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivide.h hs_ShapeUpgrade_ShapeDivide_getContext" rawGetContext
    :: Ptr ShapeDivide
    -> IO (Ptr (Handle ShapeBuild.ReShape))

getContext :: Ptr ShapeDivide -> Acquire (Ptr (Handle ShapeBuild.ReShape))
getContext divide = mkAcquire (rawGetContext divide) deleteHandleReShape

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivide.h hs_ShapeUpgrade_ShapeDivide_setPrecision" rawSetPrecision :: Ptr ShapeDivide -> CDouble -> IO ()

setPrecision :: Ptr ShapeDivide -> Double -> IO ()
setPrecision = coerce rawSetPrecision

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivide.h hs_ShapeUpgrade_ShapeDivide_setMaxTolerance" rawSetMaxTolerance :: Ptr ShapeDivide -> CDouble -> IO ()

setMaxTolerance :: Ptr ShapeDivide -> Double -> IO ()
setMaxTolerance = coerce rawSetMaxTolerance

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivide.h hs_ShapeUpgrade_ShapeDivide_setMinTolerance" rawSetMinTolerance :: Ptr ShapeDivide -> CDouble -> IO ()

setMinTolerance :: Ptr ShapeDivide -> Double -> IO ()
setMinTolerance = coerce rawSetMinTolerance
