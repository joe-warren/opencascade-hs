{-# LANGUAGE CApiFFI #-}
module OpenCascade.TDF.Internal.Destructors 
( deleteLabel
) where
import OpenCascade.TDF.Types 
import Foreign.Ptr (Ptr)


foreign import capi unsafe "hs_TDF_Label.h hs_delete_TDF_Label" deleteLabel :: Ptr Label -> IO ()
