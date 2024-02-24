{-# LANGUAGE CApiFFI #-}
module OpenCascade.GProp.Internal.Destructors 
( deleteGProps
) where

import OpenCascade.GProp.Types

import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_GProp_GProps.h hs_delete_GProp_GProps" deleteGProps :: Ptr GProps-> IO ()