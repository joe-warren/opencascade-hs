{-# LANGUAGE CApiFFI #-}
module OpenCascade.BOPAlgo.Tools
( edgesToWires
, wiresToFaces
) where

import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Exception (wrapException)
import OpenCascade.Internal.Bool (boolToCBool, cBoolToBool)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CBool (..), CDouble (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Data.Acquire (Acquire, mkAcquire)
import Data.Coerce (coerce)

foreign import capi unsafe "hs_BOPAlgo_Tools.h hs_BOPAlgo_Tools_EdgesToWires" rawEdgesToWires
    :: Ptr TopoDS.Shape
    -> CBool
    -> CDouble
    -> Ptr CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

edgesToWires :: Ptr TopoDS.Shape -> Bool -> Double -> Acquire (Int, Ptr TopoDS.Shape)
edgesToWires edges shared angTol =
    let acquire = alloca $ \statusPtr -> do
            wires <- wrapException $ rawEdgesToWires edges (boolToCBool shared) (coerce angTol) statusPtr
            status <- peek statusPtr
            return (fromIntegral status, wires)
    in mkAcquire acquire (deleteShape . snd)

foreign import capi unsafe "hs_BOPAlgo_Tools.h hs_BOPAlgo_Tools_WiresToFaces" rawWiresToFaces
    :: Ptr TopoDS.Shape
    -> CDouble
    -> Ptr CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

wiresToFaces :: Ptr TopoDS.Shape -> Double -> Acquire (Bool, Ptr TopoDS.Shape)
wiresToFaces wires angTol =
    let acquire = alloca $ \successPtr -> do
            faces <- wrapException $ rawWiresToFaces wires (coerce angTol) successPtr
            success <- peek successPtr
            return (cBoolToBool success, faces)
    in mkAcquire acquire (deleteShape . snd)
