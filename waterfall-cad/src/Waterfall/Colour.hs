module Waterfall.Colour
( Colour (..)
, colourFromRGB24
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

-- | Three sRGB values, in the range [0, 1]
data Colour = Colour Double Double Double
    deriving (Eq, Ord, Show)

colourFromRGB24 :: Word32 -> Colour
colourFromRGB24 rgb =
    let channel shift = fromIntegral ((rgb `shiftR` shift) .&. 0xFF) / 255
    in Colour (channel 16) (channel 8) (channel 0)

black, white, silver, grey, maroon, red, purple, fuchsia, green, lime, olive, yellow, navy, blue, teal, aqua :: Colour
black = colourFromRGB24 0x000000
white = colourFromRGB24 0xFFFFFF
silver = colourFromRGB24 0xC0C0C0
grey = colourFromRGB24 0x808080
maroon = colourFromRGB24 0x800000
red = colourFromRGB24 0xFF0000
purple = colourFromRGB24 0x800080
fuchsia = colourFromRGB24 0xFF00FF
green = colourFromRGB24 0x008000
lime = colourFromRGB24 0x00FF00
olive = colourFromRGB24 0x808000
yellow = colourFromRGB24 0xFFFF00
navy = colourFromRGB24 0x000080
blue = colourFromRGB24 0x0000FF
teal = colourFromRGB24 0x008080
aqua = colourFromRGB24 0x00FFFF