module Waterfall.TwoD.Shape
( Shape
, fromPath
) where

import Waterfall.TwoD.Internal.Shape (Shape (..))
import Waterfall.TwoD.Internal.Path (Path (..))
import qualified OpenCascade.BRepBuilderAPI.MakeFace as MakeFace
import OpenCascade.Inheritance (upcast)

fromPath :: Path -> Shape
fromPath (Path runPath)= Shape $ do
    p <- runPath
    upcast <$> (MakeFace.face =<< MakeFace.fromWire p False)
