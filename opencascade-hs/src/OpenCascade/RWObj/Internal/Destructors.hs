
{-# LANGUAGE CApiFFI #-}
module OpenCascade.RWObj.Internal.Destructors
( deleteCafWriter
, deleteCafReader
) where

import OpenCascade.RWObj.Types
import Foreign.Ptr 

foreign import capi unsafe "hs_RWObj_CafWriter.h hs_delete_RWObj_CafWriter" deleteCafWriter :: Ptr CafWriter -> IO ()
foreign import capi unsafe "hs_RWObj_CafReader.h hs_delete_RWObj_CafReader" deleteCafReader :: Ptr CafReader -> IO ()