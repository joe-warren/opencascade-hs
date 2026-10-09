{-# LANGUAGE CApiFFI #-}
module OpenCascade.Quantity.Internal.Destructors
( deleteColor
) where

import OpenCascade.Quantity.Types
import Foreign.Ptr

foreign import capi unsafe "hs_Quantity_Color.h hs_delete_Quantity_Color" deleteColor :: Ptr Color -> IO ()
