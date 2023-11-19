module Waterfall.TwoD.Internal.Shape
(Shape(..)
) where

import qualified OpenCascade.TopoDS as TopoDS
import Data.Acquire
import Foreign.Ptr

-- TopoDS_Wire and TopoDS_Face represent shapes in _3d space_
-- however, we're constraining this to the plane `z = 0`
-- the underlying TopoDS_Shape here should _generally_ be a TopoDS_Face
newtype Shape = Shape { runShape :: Acquire (Ptr TopoDS.Shape) }