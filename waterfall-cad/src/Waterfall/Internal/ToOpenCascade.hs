module Waterfall.Internal.ToOpenCascade
( v3ToVertex
, v3ToPnt
, v3ToDir
, shapeSingletonList
) where

import Linear (V3 (..))
import Data.Acquire (Acquire)
import Foreign.Ptr (Ptr)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.GP.Dir as GP.Dir
import qualified OpenCascade.BRepBuilderAPI.MakeVertex as MakeVertex
import qualified OpenCascade.NCollection as NCollection
import qualified OpenCascade.NCollection.List as NCollection.List
import Control.Monad.IO.Class (liftIO)


v3ToPnt :: V3 Double -> Acquire (Ptr GP.Pnt)
v3ToPnt (V3 x y z) = GP.Pnt.new x y z

v3ToDir :: V3 Double -> Acquire (Ptr GP.Dir)
v3ToDir (V3 x y z) = GP.Dir.new x y z

v3ToVertex :: V3 Double -> Acquire (Ptr TopoDS.Vertex)
v3ToVertex v = do
    pnt <- v3ToPnt v
    builder <- MakeVertex.fromPnt pnt
    MakeVertex.vertex builder

shapeSingletonList :: Ptr TopoDS.Shape -> Acquire (Ptr (NCollection.List TopoDS.Shape))
shapeSingletonList s = do
    list <- NCollection.List.newListOfShape
    liftIO $ NCollection.List.appendListOfShape list s
    return list
    