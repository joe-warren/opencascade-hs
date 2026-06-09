{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepTools.WireExplorer
( WireExplorer
, fromWire
, more
, next
, current
, orientation
)where

import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.BRepTools.Types (WireExplorer)
import OpenCascade.BRepTools.Internal.Destructors (deleteWireExplorer)
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import qualified OpenCascade.TopAbs as TopAbs
import Foreign.Ptr
import Foreign.C
import Data.Acquire
foreign import capi unsafe "hs_BRepTools_WireExplorer.h hs_new_BRepTools_WireExplorer_fromWire" rawNew
    :: Ptr TopoDS.Wire
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr WireExplorer)

fromWire :: Ptr TopoDS.Wire -> Acquire (Ptr WireExplorer)
fromWire wire = mkAcquire (wrapException $ rawNew wire) deleteWireExplorer

foreign import capi unsafe "hs_BRepTools_WireExplorer.h hs_BRepTools_WireExplorer_more" rawMore :: Ptr WireExplorer -> IO (CBool)

more :: Ptr WireExplorer -> IO Bool
more = fmap (cBoolToBool) . rawMore

foreign import capi unsafe "hs_BRepTools_WireExplorer.h hs_BRepTools_WireExplorer_next" rawNext
    :: Ptr WireExplorer
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

next :: Ptr WireExplorer -> IO ()
next explorer = wrapException $ rawNext explorer

foreign import capi unsafe "hs_BRepTools_WireExplorer.h hs_BRepTools_WireExplorer_current" rawCurrent
    :: Ptr WireExplorer
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Edge)

current :: Ptr WireExplorer -> IO (Ptr TopoDS.Edge)
current explorer = wrapException $ rawCurrent explorer

foreign import capi unsafe "hs_BRepTools_WireExplorer.h hs_BRepTools_WireExplorer_orientation" rawOrientation :: Ptr WireExplorer -> IO (CInt)

orientation :: Ptr WireExplorer -> IO TopAbs.Orientation
orientation explorer = toEnum . fromIntegral <$>  rawOrientation explorer

