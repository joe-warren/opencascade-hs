{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeUpgrade.Internal.Destructors
( deleteUnifySameDomain
) where

import OpenCascade.ShapeUpgrade.Types (UnifySameDomain)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_ShapeUpgrade_UnifySameDomain.h hs_delete_ShapeUpgrade_UnifySameDomain" deleteUnifySameDomain :: Ptr UnifySameDomain -> IO ()
