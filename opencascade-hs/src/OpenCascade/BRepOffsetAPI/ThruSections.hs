{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepOffsetAPI.ThruSections
( ThruSections
, new
, addWire
, addVertex
) where


import OpenCascade.BRepOffsetAPI.Types (ThruSections)
import OpenCascade.BRepOffsetAPI.Internal.Destructors (deleteThruSections)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr
import Foreign.C (CBool (..), CDouble (..), CInt)
import Data.Acquire
import OpenCascade.Internal.Bool (boolToCBool)
import Data.Coerce (coerce)


foreign import capi unsafe "hs_BRepOffsetAPI_ThruSections.h hs_new_BRepOffsetAPI_ThruSections" rawNew :: CBool -> CBool -> CDouble -> IO (Ptr ThruSections)

new :: Bool -> Bool -> Double -> Acquire (Ptr ThruSections)
new makeSolid ruled precision = mkAcquire (rawNew (boolToCBool makeSolid) (boolToCBool ruled) (coerce precision)) deleteThruSections

foreign import capi unsafe "hs_BRepOffsetAPI_ThruSections.h hs_BRepOffsetAPI_ThruSections_addWire" rawAddWire
    :: Ptr ThruSections
    -> Ptr TopoDS.Wire
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addWire :: Ptr ThruSections -> Ptr TopoDS.Wire -> IO ()
addWire thruSections wire = wrapException $ rawAddWire thruSections wire

foreign import capi unsafe "hs_BRepOffsetAPI_ThruSections.h hs_BRepOffsetAPI_ThruSections_addVertex" rawAddVertex
    :: Ptr ThruSections
    -> Ptr TopoDS.Vertex
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addVertex :: Ptr ThruSections -> Ptr TopoDS.Vertex -> IO ()
addVertex thruSections vertex = wrapException $ rawAddVertex thruSections vertex