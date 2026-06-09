{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopoDS.Builder 
( Builder 
, new
, makeWire
, makeShell
, makeSolid
, makeCompSolid
, makeCompound
, add
, remove
) where

import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Destructors (deleteBuilder)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_TopoDS_Builder.h hs_new_TopoDS_Builder" rawNew :: IO (Ptr Builder)

new :: Acquire (Ptr Builder)
new = mkAcquire rawNew deleteBuilder

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_makeWire" rawMakeWire
    :: Ptr Builder -> Ptr Wire -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

makeWire :: Ptr Builder -> Ptr Wire -> IO ()
makeWire builder wire = wrapException $ rawMakeWire builder wire

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_makeShell" rawMakeShell
    :: Ptr Builder -> Ptr Shell -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

makeShell :: Ptr Builder -> Ptr Shell -> IO ()
makeShell builder shell = wrapException $ rawMakeShell builder shell

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_makeSolid" rawMakeSolid
    :: Ptr Builder -> Ptr Solid -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

makeSolid :: Ptr Builder -> Ptr Solid -> IO ()
makeSolid builder solid = wrapException $ rawMakeSolid builder solid

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_makeCompSolid" rawMakeCompSolid
    :: Ptr Builder -> Ptr CompSolid -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

makeCompSolid :: Ptr Builder -> Ptr CompSolid -> IO ()
makeCompSolid builder solid = wrapException $ rawMakeCompSolid builder solid

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_makeCompound" rawMakeCompound
    :: Ptr Builder -> Ptr Compound -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

makeCompound :: Ptr Builder -> Ptr Compound -> IO ()
makeCompound builder compound = wrapException $ rawMakeCompound builder compound

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_add" rawAdd
    :: Ptr Builder -> Ptr Shape -> Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

add :: Ptr Builder -> Ptr Shape -> Ptr Shape -> IO ()
add builder s c = wrapException $ rawAdd builder s c

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_remove" rawRemove
    :: Ptr Builder -> Ptr Shape -> Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

remove :: Ptr Builder -> Ptr Shape -> Ptr Shape -> IO ()
remove builder s c = wrapException $ rawRemove builder s c
