module Waterfall.Loft
( pointedLoft
, loft
) where

import Linear (V3 (..))
import Waterfall.Internal.Path (Path, rawPath)
import Waterfall.Internal.Solid (Solid (..), solidFromAcquire)
import Waterfall.Internal.ToOpenCascade (v3ToVertex)
import qualified OpenCascade.BRepOffsetAPI.ThruSections as ThruSections
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import OpenCascade.Inheritance (upcast)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_, (<=<))

pointedLoft :: Double -> Maybe (V3 Double) -> [Path] -> Maybe (V3 Double) -> Solid
pointedLoft precision start paths end = 
    solidFromAcquire $ do
        thruSections <- ThruSections.new True False precision
        forM_ start ((liftIO . ThruSections.addVertex thruSections) <=< v3ToVertex)
        forM_ paths (liftIO . ThruSections.addWire thruSections . rawPath)
        forM_ end ((liftIO . ThruSections.addVertex thruSections) <=< v3ToVertex)
        MakeShape.shape (upcast thruSections)

loft :: Double -> [Path] -> Solid
loft precision paths = pointedLoft precision Nothing paths Nothing