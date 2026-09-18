{-# LANGUAGE DerivingVia, DeriveGeneric #-}
module Waterfall.Paint 
( Colour (..)
, Paint
, DoubleSidedness (..)
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

-- | Three Linear RGB values, in the range [0, 1]
data Colour = Colour Double Double Double
    deriving (Eq, Ord, Show)

data DoubleSidedness = SingleSided | DoubleSided 
    deriving (Eq, Ord, Show) 

-- | Can be applied to change the appearance of the surface of a `Solid`
-- 
-- `Paint` values can be constructed from the `paintWith*` functions
-- and can be combined Monoidally (with `<>`).
--
-- The Monoid instance takes the rightmost defined value of each property
data Paint = Paint 
    { _paintColour :: Last Colour
    , _paintOpacity :: Last Double
    , _paintMetallic :: Last Double
    , _paintRoughness :: Last Double
    , _paintEmissiveColour :: Last Colour
    , _paintName :: Last String
    , _paintDoubleSidedness :: Last DoubleSidedness
    } deriving (Eq, Ord, Generic)
    deriving (Semigroup, Monoid) via (Generically Paint)

instance Show Paint where
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

paintColour :: Lens' Paint (Maybe Colour)
paintColour = 
    lens
        (getLast . _paintColour) 
        (\p c -> p { _paintColour = Last c })

paintWithColour :: Colour -> Paint
paintWithColour c = mempty { _paintColour = Last (Just c) }

paintOpacity :: Lens' Paint (Maybe Double)
paintOpacity = 
    lens
        (getLast . _paintOpacity) 
        (\p c -> p { _paintOpacity = Last c })

paintWithOpacity :: Double -> Paint
paintWithOpacity o = mempty { _paintOpacity = Last (Just o) }

paintMetallic :: Lens' Paint (Maybe Double)
paintMetallic = 
    lens
        (getLast . _paintMetallic) 
        (\p c -> p { _paintMetallic = Last c })

paintWithMetallic :: Double -> Paint
paintWithMetallic m = mempty { _paintMetallic = Last (Just m) }


paintRoughness :: Lens' Paint (Maybe Double)
paintRoughness = 
    lens
        (getLast . _paintRoughness) 
        (\p c -> p { _paintRoughness = Last c })

paintWithRoughness :: Double -> Paint
paintWithRoughness r = mempty { _paintRoughness= Last (Just r) }

paintEmissiveColour :: Lens' Paint (Maybe Colour)
paintEmissiveColour = 
    lens
        (getLast . _paintEmissiveColour) 
        (\p c -> p { _paintEmissiveColour = Last c })

paintWithEmissiveColour :: Colour -> Paint
paintWithEmissiveColour c = mempty { _paintEmissiveColour = Last (Just c) }

paintName :: Lens' Paint (Maybe String)
paintName = 
    lens
        (getLast . _paintName) 
        (\p c -> p { _paintName = Last c })

paintWithName :: String -> Paint
paintWithName n = mempty { _paintName = Last (Just n) }

paintDoubleSidedness :: Lens' Paint (Maybe DoubleSidedness)
paintDoubleSidedness = 
    lens
        (getLast . _paintDoubleSidedness) 
        (\p c -> p { _paintDoubleSidedness = Last c })

paintWithDoubleSidedness :: DoubleSidedness -> Paint
paintWithDoubleSidedness ds = mempty { _paintDoubleSidedness = Last (Just ds) }