module Waterfall.Internal.ToOpenCascade
( v3ToVertex
, v3ToPnt
, v3ToDir
) where

import Linear (V3 (..))
import Data.Acquire (Acquire)
import Foreign.Ptr (Ptr)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.GP.Dir as GP.Dir
import qualified OpenCascade.BRepBuilderAPI.MakeVertex as MakeVertex


v3ToPnt :: V3 Double -> Acquire (Ptr GP.Pnt)
v3ToPnt (V3 x y z) = GP.Pnt.new x y z

v3ToDir :: V3 Double -> Acquire (Ptr GP.Dir)
v3ToDir (V3 x y z) = GP.Dir.new x y z

v3ToVertex :: V3 Double -> Acquire (Ptr TopoDS.Vertex)
v3ToVertex v = do
    pnt <- v3ToPnt v
    builder <- MakeVertex.fromPnt pnt
    MakeVertex.vertex builder

    