module Waterfall.Internal.ToOpenCascade
( v3ToVertex
) where

import Linear (V3 (..))
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.BRepBuilderAPI.MakeVertex as MakeVertex

v3ToVertex :: V3 Double -> Acquire (Ptr TopoDS.Vertex)
v3ToVertex (V3 x y z) = do
    pnt <- GP.Pnt.new x y z
    builder <- MakeVertex.fromPnt pnt
    MakeVertex.vertex builder

    