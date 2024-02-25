{-# LANGUAGE CApiFFI #-}
module OpenCascade.Bnd.Internal.Destructors 
( deleteBox
) where

import Foreign.Ptr
import OpenCascade.Bnd.Types 

foreign import capi unsafe "hs_Bnd_Box.h hs_delete_Bnd_Box" deleteBox :: Ptr Box -> IO ()