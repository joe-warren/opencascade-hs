{-|
Module: Waterfall.Loft

[Loft](https://en.wikipedia.org/wiki/Loft_\(3D\)) is a method to create smooth 3D shapes. 

Analogous to the [lofting](https://en.wikipedia.org/wiki/Lofting) process in boat building. 
A loft is defined by planar cross-sections of the desired shape at chosen locations. 
These cross-sections are then interpolated to form a smooth 3D shape.
-}
module Waterfall.Loft
( pointedLoftWithPrecision
, pointedLoft
, loft
-- * Functions that return Errors
, tryPointedLoftWithPrecision
, tryPointedLoft
, tryLoft
) where

import Linear (V3 (..))
import Waterfall.Internal.Path (Path, rawPath)
import Waterfall.Internal.Solid (Solid (..), solidFromAcquireWithCatch)
import Waterfall.Internal.ToOpenCascade (v3ToVertex)
import Waterfall.Internal.Path.Common (rawPathWire)
import qualified OpenCascade.BRepOffsetAPI.ThruSections as ThruSections
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import Data.Either (fromRight)
import OpenCascade.Inheritance (upcast)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_, (<=<))
import Waterfall.Error (WaterfallError)

-- | like `pointedLoftWithPrecision`, but returns an Either on error
tryPointedLoftWithPrecision :: Double -- ^ The loft precision, this should be a small value, e.g. @ 1e-6 @ 
    -> Maybe (V3 Double) -- ^ Optional start point for the loft
    -> [Path] -- ^ Series of cross-sections that the loft will pass through
    -> Maybe (V3 Double) -- ^ Optional end point for the loft
    -> Either WaterfallError Solid
tryPointedLoftWithPrecision precision start paths end = solidFromAcquireWithCatch $ do
    thruSections <- ThruSections.new True False precision
    forM_ start ((liftIO . ThruSections.addVertex thruSections) <=< v3ToVertex)
    forM_ paths (traverse (liftIO . ThruSections.addWire thruSections) . rawPathWire . rawPath)
    forM_ end ((liftIO . ThruSections.addVertex thruSections) <=< v3ToVertex)
    MakeShape.shape (upcast thruSections)

-- | like `pointedLoft`, but returns an Either on error
tryPointedLoft :: 
    Maybe (V3 Double) -- ^ Optional start point for the loft
    -> [Path] -- ^ Series of cross-sections that the loft will pass through
    -> Maybe (V3 Double) -- ^ Optional end point for the loft
    -> Either WaterfallError Solid
tryPointedLoft = tryPointedLoftWithPrecision defaultPrecision

-- | like `pointedLoft`, but returns an Either on error
tryLoft :: 
    [Path] -- ^ Series of cross-sections that the loft will pass through
    -> Either WaterfallError Solid
tryLoft paths = tryPointedLoft Nothing paths Nothing

defaultPrecision :: Double
defaultPrecision = 1e-6

-- | like `pointedLoft`, but allows the user to set the precision used by the underlying algorithm
pointedLoftWithPrecision :: Double -- ^ The loft precision, this should be a small value, e.g. @ 1e-6 @ 
    -> Maybe (V3 Double) -- ^ Optional start point for the loft
    -> [Path] -- ^ Series of cross-sections that the loft will pass through
    -> Maybe (V3 Double) -- ^ Optional end point for the loft
    -> Solid
pointedLoftWithPrecision precision start paths end =
    fromRight mempty $ tryPointedLoftWithPrecision precision start paths end

-- | Form a Loft which may terminate at defined points.
--
-- If the start or end points are set to `Nothing` then one end of the loft will be the terminal cross-section.
-- Otherwise, the loft will interpolate to that point.
pointedLoft :: 
    Maybe (V3 Double) -- ^ Optional start point for the loft
    -> [Path] -- ^ Series of cross-sections that the loft will pass through
    -> Maybe (V3 Double) -- ^ Optional end point for the loft
    -> Solid
pointedLoft = pointedLoftWithPrecision defaultPrecision

-- | Form a loft between a series of cross-sections.
loft :: 
    [Path] -- ^ Series of cross-sections that the loft will pass through
    -> Solid
loft paths = pointedLoft Nothing paths Nothing

