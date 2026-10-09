{-# LANGUAGE CApiFFI #-}
module OpenCascade.Quantity.Color
( Color
, new
) where

import OpenCascade.Quantity.Types (Color)
import OpenCascade.Quantity.TypeOfColor (TypeOfColor)
import OpenCascade.Quantity.Internal.Destructors (deleteColor)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CDouble (..), CInt (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_Quantity_Color.h hs_new_Quantity_Color" rawNew
    :: CDouble
    -> CDouble
    -> CDouble
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Color)

new :: Double -> Double -> Double -> TypeOfColor -> Acquire (Ptr Color)
new c1 c2 c3 theType =
    mkAcquire
        (wrapException $ rawNew (realToFrac c1) (realToFrac c2) (realToFrac c3) (fromIntegral . fromEnum $ theType))
        deleteColor
