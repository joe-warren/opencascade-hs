{-# LANGUAGE CApiFFI #-}
module OpenCascade.IntCurvesFace.Internal.Destructors
( deleteShapeIntersector
) where

import OpenCascade.IntCurvesFace.Types
import Foreign.Ptr

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_delete_IntCurvesFace_ShapeIntersector" deleteShapeIntersector :: Ptr ShapeIntersector -> IO ()
