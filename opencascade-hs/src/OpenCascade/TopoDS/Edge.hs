{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopoDS.Edge
( Edge
, new
) where 

import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Destructors
import Foreign.Ptr
import Data.Acquire 

-- new

foreign import capi unsafe "hs_TopoDS_Edge.h hs_new_TopoDS_Edge" rawNew :: IO (Ptr Edge)

new :: Acquire (Ptr Edge)
new = mkAcquire rawNew (deleteShape . castPtr)
