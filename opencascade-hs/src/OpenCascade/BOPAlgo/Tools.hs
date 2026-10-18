{-# LANGUAGE CApiFFI #-}
module OpenCascade.BOPAlgo.Tools
( edgesToWires
, wiresToFaces
) where

import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.Internal.Exception (wrapException)
import OpenCascade.Internal.Bool (boolToCBool, cBoolToBool)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CBool (..), CDouble (..))
import Data.Coerce (coerce)

foreign import capi unsafe "hs_BOPAlgo_Tools.h hs_BOPAlgo_Tools_EdgesToWires" rawEdgesToWires
    :: Ptr TopoDS.Shape
    -> Ptr TopoDS.Shape
    -> CBool
    -> CDouble
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CInt

edgesToWires :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Bool -> Double -> IO Int
edgesToWires edges wires shared angTol =
    fmap fromIntegral . wrapException $ rawEdgesToWires edges wires (boolToCBool shared) (coerce angTol) 

foreign import capi unsafe "hs_BOPAlgo_Tools.h hs_BOPAlgo_Tools_WiresToFaces" rawWiresToFaces
    :: Ptr TopoDS.Shape
    -> Ptr TopoDS.Shape
    -> CDouble
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CBool

wiresToFaces :: Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Double -> IO Bool
wiresToFaces wires faces angTol =
    fmap cBoolToBool . wrapException $ rawWiresToFaces wires faces (coerce angTol)
