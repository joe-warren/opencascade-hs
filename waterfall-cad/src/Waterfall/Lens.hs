{-# LANGUAGE RankNTypes #-}
module Waterfall.Lens
( _translated
, _scaled
, _uScaled
, _rotated
, _mirrored
, _translated2D
, _scaled2D
, _uScaled2D
, _rotated2D
, _mirrored2D
, _centerOfMass
, _axisAlignedBoundingBox
, _axisAlignedBoundingBoxMin
, _axisAlignedBoundingBoxMax
, _axisAlignedBoundingBoxCenter
, _masked
, place
) where

import Control.Lens
import Linear
import qualified Waterfall.Solids as Solids
import qualified Waterfall.Booleans as Booleans
import Waterfall.BoundingBox.AxisAligned (axisAlignedBoundingBox)
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.TwoD.Transforms as TwoD.Transforms

toTranslationLens :: Transforms.Transformable s => (s -> V3 Double) -> Lens' s (V3 Double)
toTranslationLens getter f s = 
    let p = getter s
    in (`Transforms.translate` s) . subtract p <$> f p

toTranslationLensMay :: Transforms.Transformable s => (s -> Maybe (V3 Double)) -> Traversal' s (V3 Double)
toTranslationLensMay getter f s = 
    case getter s of
        Nothing -> pure s
        Just p -> (`Transforms.translate` s) . subtract p <$> f p

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

_translated2D :: TwoD.Transforms.Transformable2D t => V2 Double -> Iso' t t
_translated2D v = iso (TwoD.Transforms.translate2D v) (TwoD.Transforms.translate2D (negate v))

_scaled2D :: TwoD.Transforms.Transformable2D t => V2 Double -> Iso' t t 
_scaled2D v = iso (TwoD.Transforms.scale2D v) (TwoD.Transforms.scale2D (1/v))

_uScaled2D :: TwoD.Transforms.Transformable2D t => Double -> Iso' t t 
_uScaled2D s = iso (TwoD.Transforms.uScale2D s) (TwoD.Transforms.uScale2D (1/s))

_rotated2D :: TwoD.Transforms.Transformable2D t => Double -> Iso' t t 
_rotated2D angle = iso (TwoD.Transforms.rotate2D angle) (TwoD.Transforms.rotate2D (negate angle))

_mirrored2D :: TwoD.Transforms.Transformable2D t => V2 Double -> Iso' t t
_mirrored2D v = let f = TwoD.Transforms.mirror2D v in iso f f 

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

_axisAlignedBoundingBoxMin :: Traversal' Solids.Solid (V3 Double)
_axisAlignedBoundingBoxMin = toTranslationLensMay (fmap fst . axisAlignedBoundingBox)

_axisAlignedBoundingBoxMax :: Traversal' Solids.Solid (V3 Double)
_axisAlignedBoundingBoxMax = toTranslationLensMay (fmap snd . axisAlignedBoundingBox)

_axisAlignedBoundingBoxCenter :: Traversal' Solids.Solid (V3 Double)
_axisAlignedBoundingBoxCenter = 
    let mean (a, b) = (a + b) / 2
     in toTranslationLensMay (fmap mean . axisAlignedBoundingBox)

-- | Lens over the parts of a `Solid` contained by another Solid
--
-- such that 
--
-- @ view (_masked a) b == intersection a b @
--
-- Notably, this isn't lawful under the lens laws
_masked :: Solids.Solid -> Lens' Solids.Solid Solids.Solid
_masked mask f s = 
    let target = s `Booleans.intersection` mask
        outside = s `Booleans.difference` mask
    in (<> outside) <$> f target

place :: Monoid s => Getter s p -> Setter' s p -> s -> s -> s
place l1 l2 s1 s2 = s1 <> (s2 & l2 .~ (s1 ^. l1))