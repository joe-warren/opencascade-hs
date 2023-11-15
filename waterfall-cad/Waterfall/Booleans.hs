module Waterfall.Booleans
( union
, difference
, intersection
) where

import qualified Waterfall.Solids as Solids
import qualified OpenCascade.BRepAlgoAPI.Fuse as Fuse
import qualified OpenCascade.BRepAlgoAPI.Cut as Cut
import qualified OpenCascade.BRepAlgoAPI.Common as Common
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.Ptr
import Data.Acquire

toBoolean :: (Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)) -> Solids.Solid -> Solids.Solid -> Solids.Solid
toBoolean f (Solids.Solid runA) (Solids.Solid runB) = Solids.Solid $ do
    a <- runA
    b <- runB
    f a b

union :: Solids.Solid -> Solids.Solid -> Solids.Solid
union = toBoolean Fuse.fuse

difference :: Solids.Solid -> Solids.Solid -> Solids.Solid
difference = toBoolean Cut.cut

intersection :: Solids.Solid -> Solids.Solid -> Solids.Solid
intersection = toBoolean Common.common
