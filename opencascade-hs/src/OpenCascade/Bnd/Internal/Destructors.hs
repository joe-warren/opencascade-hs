{-# LANGUAGE CApiFFI #-}
module OpenCascade.Bnd.Internal.Destructors 
( deleteBox
, deleteOBB
) where

import Foreign.Ptr
import OpenCascade.Bnd.Types 

foreign import capi unsafe "hs_Bnd_Box.h hs_delete_Bnd_Box" deleteBox :: Ptr Box -> IO ()
foreign import capi unsafe "hs_Bnd_OBB.h hs_delete_Bnd_OBB" deleteOBB :: Ptr OBB -> IO ()