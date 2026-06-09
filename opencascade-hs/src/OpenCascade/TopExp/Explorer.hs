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
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_TopExp_Explorer.h hs_new_TopExp_Explorer" rawNew
    :: Ptr TopoDS.Shape
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Explorer)

new :: Ptr TopoDS.Shape -> TopAbs.ShapeEnum -> Acquire (Ptr Explorer)
new shape theType = mkAcquire (wrapException $ rawNew shape (fromIntegral . fromEnum $ theType)) deleteExplorer

foreign import capi unsafe "hs_TopExp_Explorer.h hs_TopExp_Explorer_more" rawMore :: Ptr Explorer -> IO (CBool)

more :: Ptr Explorer -> IO Bool
more = fmap (cBoolToBool) . rawMore

foreign import capi unsafe "hs_TopExp_Explorer.h hs_TopExp_Explorer_next" rawNext
    :: Ptr Explorer
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

next :: Ptr Explorer -> IO ()
next explorer = wrapException $ rawNext explorer

foreign import capi unsafe "hs_TopExp_Explorer.h hs_TopExp_Explorer_value" rawValue
    :: Ptr Explorer
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

value :: Ptr Explorer -> IO (Ptr TopoDS.Shape)
value explorer = wrapException $ rawValue explorer