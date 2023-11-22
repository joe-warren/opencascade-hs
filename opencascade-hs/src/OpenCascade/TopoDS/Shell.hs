{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopoDS.Shell
( Shell
, new
) where 

import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Destructors
import Foreign.Ptr
import Data.Acquire 

-- new

foreign import capi unsafe "hs_TopoDS_Shell.h hs_new_TopoDS_Shell" rawNew :: IO (Ptr Shell)

new :: Acquire (Ptr Shell)
new = mkAcquire rawNew (deleteShape . castPtr)
