{-#LANGUAGE DeriveGeneric #-}
module Waterfall.Colour
( Colour (..)
, toDataColour
, fromDataColour
, sRGBComponents
, rgb24
, mixColours
, mixColoursRGB
, mixColoursCIE
, black
, white
, silver
, grey
, maroon
, red
, purple
, fuchsia
, green
, lime
, olive
, yellow 
, navy
, blue
, teal
, aqua
) where

import Data.Word (Word32)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Numeric (showHex)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import qualified Data.Colour
import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.Ok as Ok
import qualified Data.Colour.CIE as CIE
import qualified Data.Colour.CIE.Illuminant as CIE.Illuminant
import Linear (V3 (..), (^*))
import Data.List.NonEmpty (NonEmpty)

-- | Three sRGB values, in the range [0, 1]
data Colour = Colour Double Double Double
    deriving (Eq, Ord, Generic)

instance NFData Colour

fromDataColour :: Data.Colour.Colour Double -> Colour
fromDataColour dc = case SRGB.toSRGB dc of 
    SRGB.RGB r g b -> Colour r g b

toDataColour :: Colour -> Data.Colour.Colour Double
toDataColour (Colour r g b) = SRGB.sRGB r g b

sRGBComponents :: Colour -> V3 Double
sRGBComponents (Colour r g b) = V3 r g b
    
instance Show Colour where
    showsPrec d c@(Colour r g b) = showParen (d > 10) $
        let SRGB.RGB r8 g8 b8 = SRGB.toSRGB24 (toDataColour c)
            word = (fromIntegral r8 `shiftL` 16) .|. (fromIntegral g8 `shiftL` 8) .|. fromIntegral b8 :: Word32
            hex = showHex word ""
        in if rgb24 word == c
              then showString "rgb24 0x"
                    . showString (replicate (6 - length hex) '0')
                    . showString hex
              else showString "Colour " 
                    . showsPrec 11 r
                    . showChar ' ' 
                    . showsPrec 11 g
                    . showChar ' '
                    . showsPrec 11 b

-- | Convert an integer into a Colour
--
-- This is useful because it lets you write hex codes, e.g.
-- 
-- @
-- rgb24 0xFFFFFF == white
-- @
rgb24 :: Word32 -> Colour
rgb24 word =
    let channel shift = fromIntegral ((word `shiftR` shift) .&. 0xFF) / 255
    in Colour (channel 16) (channel 8) (channel 0)

mixColours :: NonEmpty Colour -> Colour
mixColours cs =
    let toLAB c = 
            let (l, a, b) = Ok.okLabView . toDataColour $ c
            in V3 l a b
        fromLAB (V3 l a b) = fromDataColour $ Ok.okLab l a b
        weight = 1 / realToFrac (length cs)
    in fromLAB
        . (^* weight)
        . sum
        . fmap toLAB
        $ cs
        
mixColoursCIE :: NonEmpty Colour -> Colour
mixColoursCIE cs =
    let chroma = CIE.Illuminant.d65
        toCIE c = 
            let (l, a, b) = CIE.cieLABView chroma . toDataColour $ c
            in V3 l a b
        fromCIE (V3 l a b) = fromDataColour $ CIE.cieLAB chroma l a b
        weight = 1 / realToFrac (length cs)
    in fromCIE 
        . (^* weight)
        . sum
        . fmap (toCIE)
        $ cs

mixColoursRGB :: NonEmpty Colour -> Colour
mixColoursRGB cs =
    let fromRGB (V3 x y z) = Colour x y z
        weight = 1 / realToFrac (length cs)
    in fromRGB
        . (^* weight)
        . sum
        . fmap (sRGBComponents)
        $ cs

black, white, silver, grey, maroon, red, purple, fuchsia, green, lime, olive, yellow, navy, blue, teal, aqua :: Colour
-- | <<doc-images/black.svg black>>
black = rgb24 0x000000
-- | <<doc-images/white.svg white>>
white = rgb24 0xFFFFFF
-- | <<doc-images/silver.svg silver>>
silver = rgb24 0xC0C0C0
-- | <<doc-images/grey.svg grey>>
grey = rgb24 0x808080
-- | <<doc-images/maroon.svg maroon>>
maroon = rgb24 0x800000
-- | <<doc-images/red.svg red>>
red = rgb24 0xFF0000
-- | <<doc-images/purple.svg purple>>
purple = rgb24 0x800080
-- | <<doc-images/fuchsia.svg fuchsia>>
fuchsia = rgb24 0xFF00FF
-- | <<doc-images/green.svg green>>
green = rgb24 0x008000
-- | <<doc-images/lime.svg lime>>
lime = rgb24 0x00FF00
-- | <<doc-images/olive.svg olive>>
olive = rgb24 0x808000
-- | <<doc-images/yellow.svg yellow>>
yellow = rgb24 0xFFFF00
-- | <<doc-images/navy.svg navy>>
navy = rgb24 0x000080
-- | <<doc-images/blue.svg blue>>
blue = rgb24 0x0000FF
-- | <<doc-images/teal.svg teal>>
teal = rgb24 0x008080
-- | <<doc-images/aqua.svg aqua>>
aqua = rgb24 0x00FFFF