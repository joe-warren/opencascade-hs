module Waterfall.TwoD.Shape
( Shape
, fromPath
) where

import Waterfall.TwoD.Internal.Shape (Shape (..))
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import qualified OpenCascade.BRepBuilderAPI.MakeFace as MakeFace
import OpenCascade.Inheritance (upcast)

fromPath :: Path2D -> Shape
fromPath (Path2D runPath)= Shape $ do
    p <- runPath
    upcast <$> (MakeFace.face =<< MakeFace.fromWire p False)
