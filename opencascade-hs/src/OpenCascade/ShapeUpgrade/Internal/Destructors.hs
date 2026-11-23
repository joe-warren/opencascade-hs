{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeUpgrade.Internal.Destructors
( deleteUnifySameDomain
, deleteShapeDivideContinuity
) where

import OpenCascade.ShapeUpgrade.Types (UnifySameDomain, ShapeDivideContinuity)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_ShapeUpgrade_UnifySameDomain.h hs_delete_ShapeUpgrade_UnifySameDomain" deleteUnifySameDomain :: Ptr UnifySameDomain -> IO ()

foreign import capi unsafe "hs_ShapeUpgrade_ShapeDivideContinuity.h hs_delete_ShapeUpgrade_ShapeDivideContinuity" deleteShapeDivideContinuity :: Ptr ShapeDivideContinuity -> IO ()
