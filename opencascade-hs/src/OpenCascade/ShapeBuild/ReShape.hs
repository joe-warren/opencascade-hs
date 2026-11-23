{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeBuild.ReShape
( ReShape
, apply
, isRecorded
) where

import OpenCascade.ShapeBuild.Types (ReShape)
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.TopAbs.ShapeEnum as TopAbs.ShapeEnum
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CBool (..))

foreign import capi unsafe "hs_ShapeBuild_ReShape.h hs_ShapeBuild_ReShape_apply" rawApply
    :: Ptr (Handle ReShape)
    -> Ptr TopoDS.Shape
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

apply :: Ptr (Handle ReShape) -> Ptr TopoDS.Shape -> TopAbs.ShapeEnum.ShapeEnum -> Acquire (Ptr TopoDS.Shape)
apply reShape shape untilType =
    mkAcquire
        (wrapException $ rawApply reShape shape (fromIntegral . fromEnum $ untilType))
        deleteShape

foreign import capi unsafe "hs_ShapeBuild_ReShape.h hs_ShapeBuild_ReShape_isRecorded" rawIsRecorded
    :: Ptr (Handle ReShape)
    -> Ptr TopoDS.Shape
    -> IO CBool

isRecorded :: Ptr (Handle ReShape) -> Ptr TopoDS.Shape -> IO Bool
isRecorded reShape shape = cBoolToBool <$> rawIsRecorded reShape shape
