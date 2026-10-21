{-#LANGUAGE DeriveGeneric #-}
module Waterfall.Colour
( Colour (..)
, rgb24
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
import Data.Bits (shiftR, (.&.))
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Three sRGB values, in the range [0, 1]
data Colour = Colour Double Double Double
    deriving (Eq, Ord, Show, Generic)

instance NFData Colour

-- | Convert an integer into a Colour
--
-- This is useful because it lets you write hex codes, e.g.
-- 
-- @
-- rgb24 0xFFFFFF == white
-- @
rgb24 :: Word32 -> Colour
rgb24 rgb =
    let channel shift = fromIntegral ((rgb `shiftR` shift) .&. 0xFF) / 255
    in Colour (channel 16) (channel 8) (channel 0)

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