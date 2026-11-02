{-#LANGUAGE DeriveGeneric #-}
module Waterfall.Colour
( Colour
, toDataColour
, fromDataColour
, sRGBComponents
, rgb24
, rgb
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
import Control.DeepSeq (NFData, rnf)
import Data.Function (on)
import qualified Data.Colour
import qualified Data.Colour.SRGB as SRGB
import Linear (V3 (..))

-- | Three sRGB values, in the range [0, 1]
newtype Colour = Colour
    { toDataColour :: (Data.Colour.Colour Double)
    }

fromDataColour :: Data.Colour.Colour Double -> Colour
fromDataColour = Colour

sRGBComponents :: Colour -> V3 Double
sRGBComponents (Colour c) = case SRGB.toSRGB c of 
    SRGB.RGB r g b -> V3 r g b

instance Eq Colour where
    (==) = (==) `on` sRGBComponents

instance Ord Colour where
    compare = compare `on` sRGBComponents

instance NFData Colour where
    rnf (Colour c) = case SRGB.toSRGB c of
        SRGB.RGB r g b -> rnf r `seq` rnf g `seq` rnf b

instance Show Colour where
    showsPrec d c = showParen (d > 10) $
        let V3 r g b = sRGBComponents c
            SRGB.RGB r8 g8 b8 = SRGB.toSRGB24 (toDataColour c)
            word = (fromIntegral r8 `shiftL` 16) .|. (fromIntegral g8 `shiftL` 8) .|. fromIntegral b8 :: Word32
            hex = showHex word ""
        in if rgb24 word == c
              then showString "rgb24 0x"
                    . showString (replicate (6 - length hex) '0')
                    . showString hex
              else showString "rgb " 
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
    in Colour $ SRGB.sRGB (channel 16) (channel 8) (channel 0)

rgb :: Double -> Double -> Double -> Colour
rgb r g b = Colour $ SRGB.sRGB r g b

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