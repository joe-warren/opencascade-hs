{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopoDS.Face
( Face
, new
) where 

import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Destructors
import Foreign.Ptr
import Data.Acquire 

-- new

foreign import capi unsafe "hs_TopoDS_Face.h hs_new_TopoDS_Face" rawNew :: IO (Ptr Face)

new :: Acquire (Ptr Face)
new = mkAcquire rawNew (deleteShape . castPtr)
