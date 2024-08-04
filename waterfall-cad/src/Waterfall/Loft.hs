{-|
Module: Waterfall.Loft

[Loft](https://en.wikipedia.org/wiki/Loft_\(3D\)) is a method to create smooth 3D shapes. 

Analagous to the [lofting](https://en.wikipedia.org/wiki/Lofting) process in boat building. 
A loft is defined by planar cross-sections of the desired shape at chosen locations. 
These cross-sections are then interpolated to form a smooth 3d shape.
-}
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

-- | Form a Loft which may terminate at defined points.
--
-- If the start or end points are set to `Nothing` then one end of the loft will be the terminal cross-section.
-- Otherwise, the loft will interpolate to that point.
pointedLoft :: Double -- ^ The loft precision, this should be a small value, e.g. @ 1e-6 @ 
    -> Maybe (V3 Double) -- ^ Optional start point for the loft
    -> [Path] -- ^ Series of cross-sections that the loft will pass through
    -> Maybe (V3 Double) -- ^ Optional end point for the loft
    -> Solid
pointedLoft precision start paths end = 
    solidFromAcquire $ do
        thruSections <- ThruSections.new True False precision
        forM_ start ((liftIO . ThruSections.addVertex thruSections) <=< v3ToVertex)
        forM_ paths (liftIO . ThruSections.addWire thruSections . rawPath)
        forM_ end ((liftIO . ThruSections.addVertex thruSections) <=< v3ToVertex)
        MakeShape.shape (upcast thruSections)

-- | Form a loft between a series of cross-sections.
loft :: Double  -- ^ The loft precision, this should be a small value, e.g @ 1e-6 @
    -> [Path] -- ^ Series of cross-sections that the loft will pass through
    -> Solid
loft precision paths = pointedLoft precision Nothing paths Nothing