{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeBuild.Internal.Destructors
( deleteHandleReShape
) where

import OpenCascade.ShapeBuild.Types (ReShape)
import OpenCascade.Handle (Handle)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_ShapeBuild_ReShape.h hs_delete_Handle_ShapeBuild_ReShape" deleteHandleReShape :: Ptr (Handle ReShape) -> IO ()
