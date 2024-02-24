module Waterfall.TwoD.Internal.Shape
(Shape(..)
) where

import qualified OpenCascade.TopoDS as TopoDS
import Foreign.Ptr

-- | A Region in 2D Space 
-- 
-- In general, this is used as a face, and extruded along some sort of path
--
-- Under the hood, this is represented by an OpenCascade `TopoDS.Shape`
-- 
-- This should be of type `TopoDS.Face`, constrained to the plane \( z=0 \).
--
-- Please feel free to report a bug if you're able to construct a `Shape`
-- which does not lie on this plane (without using Internal functions).
-- Or which is not either a `TopoDS.Face`, or a composite of faces.
newtype Shape = Shape { rawShape :: Ptr TopoDS.Shape }