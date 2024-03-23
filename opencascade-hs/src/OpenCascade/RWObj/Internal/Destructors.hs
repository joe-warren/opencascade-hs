
{-# LANGUAGE CApiFFI #-}
module OpenCascade.RWObj.Internal.Destructors
( deleteCafWriter
) where

import OpenCascade.RWObj.Types
import Foreign.Ptr 

foreign import capi unsafe "hs_RWObj_CafWriter.h hs_delete_RWObj_CafWriter" deleteCafWriter :: Ptr CafWriter -> IO ()