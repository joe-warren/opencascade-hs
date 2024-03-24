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
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_TopoDS_Builder.h hs_new_TopoDS_Builder" rawNew :: IO (Ptr Builder)

new :: Acquire (Ptr Builder)
new = mkAcquire rawNew deleteBuilder

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_makeWire" makeWire :: Ptr Builder -> Ptr Wire -> IO ()

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_makeShell" makeShell :: Ptr Builder -> Ptr Shell -> IO ()

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_makeSolid" makeSolid :: Ptr Builder -> Ptr Solid -> IO ()

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_makeCompSolid" makeCompSolid :: Ptr Builder -> Ptr CompSolid -> IO ()

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_makeCompound" makeCompound :: Ptr Builder -> Ptr Compound -> IO ()

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_add" add :: Ptr Builder -> Ptr Shape -> Ptr Shape -> IO ()

foreign import capi unsafe "hs_TopoDS_Builder.h hs_TopoDS_Builder_remove" remove :: Ptr Builder -> Ptr Shape -> Ptr Shape -> IO ()
