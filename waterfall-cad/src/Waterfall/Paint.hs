{-# LANGUAGE DerivingVia, DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-|
Module: Waterfall.Paint

`Paint` can be applied to change the appearance of the surface of a `Waterfall.Solids.Solid` (with `Waterfall.Solids.paintSolid`).

`Paint` values can be constructed from the @paintWith*@ functions and can be combined monoidally (with `<>`), e.g.

@
redShinyPaint =
    paintWithColour (Colour 1 0 0)
    <> paintWithRoughness 0.01
    <> paintWithMetallic 1
    <> paintWithDoubleSidedness SingleSided
@

Not all `Paint` properties are supported by every file format:

* glTF and OBJ use the full set
* STEP only uses the colour
* SVG uses colour and opacity
-}
module Waterfall.Paint 
( Colour (..)
, Paint
, DoubleSidedness (..)
, defaultPaint
, paintIsDefined
-- * Constructors
, paintWithColour
, paintWithOpacity
, paintWithMetallic
, paintWithRoughness
, paintWithEmissiveColour
, paintWithName
, paintWithDoubleSidedness
-- * Optics
, paintColour
, paintOpacity
, paintMetallic
, paintRoughness
, paintEmissiveColour
, paintName
, paintDoubleSidedness
) where
import Data.Monoid (Last (..), Endo (..))
import GHC.Generics (Generically (..), Generic)
import Data.Maybe (catMaybes)
import Data.List (intersperse)
import Control.Lens (Lens', lens)
import Waterfall.Colour (Colour (..))
import Control.DeepSeq (NFData (rnf), rwhnf)

-- | Whether to display a surface as Double or Single Sided
--
-- In most formats and viewers this will be implemented using backface culling 
data DoubleSidedness = SingleSided | DoubleSided 
    deriving (Eq, Ord, Show) 

instance NFData DoubleSidedness where rnf = rwhnf

-- | Can be applied to change the appearance of the surface of a `Solid`
-- 
-- `Paint` values can be constructed from the @paintWith*@ functions
-- and can be combined monoidally (with `<>`).
data Paint = Paint 
    { _paintColour :: Last Colour
    , _paintOpacity :: Last Double
    , _paintMetallic :: Last Double
    , _paintRoughness :: Last Double
    , _paintEmissiveColour :: Last Colour
    , _paintName :: Last String
    , _paintDoubleSidedness :: Last DoubleSidedness
    } deriving (Eq, Ord, Generic)
    deriving 
        ( Semigroup -- ^ `(<>)` takes the rightmost defined value of each property.
        , Monoid
        ) via (Generically Paint)

instance NFData Paint

instance Show Paint where
  showsPrec :: Int -> Paint -> ShowS
  showsPrec d p = 
    let term name field = 
            let fv v = 
                    showString name
                     . showChar ' '
                     . showsPrec 11 v 
            in fv <$> getLast (field p)
        terms = catMaybes 
            [ term "paintWithColour" _paintColour
            , term "paintWithOpacity" _paintOpacity
            , term "paintWithMetallic" _paintMetallic
            , term "paintWithRoughness" _paintRoughness
            , term "paintWithEmissiveColour" _paintEmissiveColour
            , term "paintWithName" _paintName
            , term "paintWithDoubleSidedness" _paintDoubleSidedness
            ]
    in case terms of
        [] -> showString "mempty"
        _ -> showParen (d > 6) . appEndo
            . foldMap Endo
            . intersperse (showString " <> ") 
            $ terms

-- | Fallback values used when a paint is unset
--
-- @
-- defaultPaint /= mempty
-- @
defaultPaint :: Paint
defaultPaint = paintWithColour (Colour 1 1 1)
    <> paintWithOpacity 1 
    <> paintWithDoubleSidedness DoubleSided
    <> paintWithMetallic 0
    <> paintWithRoughness 0.5

-- | Test whether a paint is storing any values
paintIsDefined :: Paint -> Bool
paintIsDefined = (/= mempty)

-- | The paint colour
paintColour :: Lens' Paint (Maybe Colour)
paintColour = 
    lens
        (getLast . _paintColour) 
        (\p c -> p { _paintColour = Last c })

-- | Create a paint with a given colour
paintWithColour :: Colour -> Paint
paintWithColour c = mempty { _paintColour = Last (Just c) }

-- | The paint opacity
paintOpacity :: Lens' Paint (Maybe Double)
paintOpacity = 
    lens
        (getLast . _paintOpacity) 
        (\p c -> p { _paintOpacity = Last c })

-- | Create a paint with a given opacity
--
-- Opacity is in the range [0, 1]
paintWithOpacity :: Double -> Paint
paintWithOpacity o = mempty { _paintOpacity = Last (Just o) }

-- | The paint "Metalness" value, in a [PBR](https://en.wikipedia.org/wiki/Physically_based_rendering) model.
paintMetallic :: Lens' Paint (Maybe Double)
paintMetallic = 
    lens
        (getLast . _paintMetallic) 
        (\p c -> p { _paintMetallic = Last c })

-- | Create a paint with a given "Metalness"
--
-- In the range [0, 1], 1 is fully metal.
paintWithMetallic :: Double -> Paint
paintWithMetallic m = mempty { _paintMetallic = Last (Just m) }


-- | The paint "Roughness" value, in a [PBR](https://en.wikipedia.org/wiki/Physically_based_rendering) model.
paintRoughness :: Lens' Paint (Maybe Double)
paintRoughness = 
    lens
        (getLast . _paintRoughness) 
        (\p c -> p { _paintRoughness = Last c })

-- | Create a paint with a given roughness
-- 
-- In the range [0, 1], 0 is perfectly smooth, 1 fully diffuse
paintWithRoughness :: Double -> Paint
paintWithRoughness r = mempty { _paintRoughness= Last (Just r) }

-- | The paint "Emissive" factor, in a [PBR](https://en.wikipedia.org/wiki/Physically_based_rendering) model.
--
-- EmissiveColour is written as linear RGB, rather than converted to sRGB.
paintEmissiveColour :: Lens' Paint (Maybe Colour)
paintEmissiveColour = 
    lens
        (getLast . _paintEmissiveColour) 
        (\p c -> p { _paintEmissiveColour = Last c })

-- | Create a paint with a given emissive factor
paintWithEmissiveColour :: Colour -> Paint
paintWithEmissiveColour c = mempty { _paintEmissiveColour = Last (Just c) }

-- | The paint name
paintName :: Lens' Paint (Maybe String)
paintName = 
    lens
        (getLast . _paintName) 
        (\p c -> p { _paintName = Last c })

-- | Create a paint with a given name
paintWithName :: String -> Paint
paintWithName n = mempty { _paintName = Last (Just n) }

-- | Whether to display a surface as Double or Single Sided
paintDoubleSidedness :: Lens' Paint (Maybe DoubleSidedness)
paintDoubleSidedness = 
    lens
        (getLast . _paintDoubleSidedness) 
        (\p c -> p { _paintDoubleSidedness = Last c })

-- | Create a paint with a given `DoubleSidedness`
paintWithDoubleSidedness :: DoubleSidedness -> Paint
paintWithDoubleSidedness ds = mempty { _paintDoubleSidedness = Last (Just ds) }
