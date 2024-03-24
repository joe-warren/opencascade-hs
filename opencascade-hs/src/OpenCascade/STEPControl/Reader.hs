{-# LANGUAGE CApiFFI #-}
module OpenCascade.STEPControl.Reader
( Reader
, new
) where

import OpenCascade.STEPControl.Types (Reader)
import OpenCascade.STEPControl.Internal.Destructors (deleteReader)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)


foreign import capi unsafe "hs_STEPControl_Reader.h hs_new_STEPControl_Reader" rawNew :: IO (Ptr Reader)

new :: Acquire (Ptr Reader)
new = mkAcquire rawNew deleteReader