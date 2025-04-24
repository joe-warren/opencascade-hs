{-# LANGUAGE RankNTypes #-}
module Waterfall.Lenses
( _translated
, _scaled
, _uScaled
, _rotated
, _mirrored
, _centerOfMass
, _axisAlignedBoundingBox
, _masked
) where

import Control.Lens
import Linear
import qualified Waterfall.Solids as Solids
import qualified Waterfall.Booleans as Booleans
import Waterfall.BoundingBox.AxisAligned (axisAlignedBoundingBox)
import qualified Waterfall.Transforms as Transforms

toTranslationLens :: Transforms.Transformable s => (s -> V3 Double) -> Lens' s (V3 Double)
toTranslationLens getter f s = 
    let p = getter s
    in (`Transforms.translate` s) . subtract p <$> f p

_centerOfMass :: Lens' Solids.Solid (V3 Double)
_centerOfMass = toTranslationLens Solids.centerOfMass

_translated :: Transforms.Transformable t => V3 Double -> Iso' t t
_translated v = iso (Transforms.translate v) (Transforms.translate (negate v))

_scaled :: Transforms.Transformable t => V3 Double -> Iso' t t
_scaled v = iso (Transforms.scale v) (Transforms.scale (1/v))

_uScaled :: Transforms.Transformable t => Double -> Iso' t t
_uScaled s = iso (Transforms.uScale s) (Transforms.uScale (1/s))

_rotated :: Transforms.Transformable t => V3 Double -> Double -> Iso' t t
_rotated axis angle = iso (Transforms.rotate axis angle) (Transforms.rotate axis (negate angle))

_mirrored :: Transforms.Transformable t => V3 Double -> Iso' t t
_mirrored v = let f = Transforms.mirror v in iso f f 

_axisAlignedBoundingBox :: Traversal' Solids.Solid (V3 Double, V3 Double)
_axisAlignedBoundingBox f s = 
    case axisAlignedBoundingBox s of
        Nothing -> pure s
        Just bb@(lo, hi) ->
            let translatedS (lo', hi') = s
                    & Transforms.translate (negate lo) 
                    & Transforms.scale ((hi' - lo') /  (hi - lo)) 
                    & Transforms.translate lo'
            in translatedS <$> f bb

_masked :: Solids.Solid -> Lens' Solids.Solid Solids.Solid
_masked mask f s = 
    let target = s `Booleans.intersection` mask
        outside = s `Booleans.difference` mask
    in (<> outside) <$> f target
