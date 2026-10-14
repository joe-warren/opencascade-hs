{-# LANGUAGE CApiFFI #-}
module OpenCascade.Quantity.ColorRGBA
( ColorRGBA
, new
) where

import OpenCascade.Quantity.Types (Color, ColorRGBA)
import OpenCascade.Quantity.Internal.Destructors (deleteColorRGBA)
import Foreign.Ptr (Ptr)
import Foreign.C (CDouble (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_Quantity_ColorRGBA.h hs_new_Quantity_ColorRGBA" rawNew :: Ptr Color -> CDouble -> IO (Ptr ColorRGBA)

new :: Ptr Color -> Double -> Acquire (Ptr ColorRGBA)
new color alpha = mkAcquire (rawNew color (realToFrac alpha)) deleteColorRGBA
