{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopExp.Explorer
( Explorer
, new
, more
, next
, value
) where

import OpenCascade.TopExp.Types (Explorer)
import OpenCascade.TopExp.Internal.Destructors (deleteExplorer)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopAbs as TopAbs
import Data.Acquire
import Foreign.Ptr
import Foreign.C
import OpenCascade.Internal.Bool (cBoolToBool)

foreign import capi unsafe "hs_TopExp_Explorer.h hs_new_TopExp_Explorer" rawNew :: Ptr TopoDS.Shape -> CInt -> IO (Ptr Explorer)

new :: Ptr TopoDS.Shape -> TopAbs.ShapeEnum -> Acquire (Ptr Explorer)
new shape theType = mkAcquire (rawNew shape (fromIntegral . fromEnum $ theType)) deleteExplorer

foreign import capi unsafe "hs_TopExp_Explorer.h hs_TopExp_Explorer_more" rawMore :: Ptr Explorer -> IO (CBool)

more :: Ptr Explorer -> IO Bool
more = fmap (cBoolToBool) . rawMore 

foreign import capi unsafe "hs_TopExp_Explorer.h hs_TopExp_Explorer_next" next :: Ptr Explorer -> IO ()

foreign import capi unsafe "hs_TopExp_Explorer.h hs_TopExp_Explorer_value" value :: Ptr Explorer -> IO (Ptr TopoDS.Shape)