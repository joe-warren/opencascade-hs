module OpenCascade.XCAFDoc.ColorType
( ColorType (..)
) where

-- this should match the enumeration in XCAFDoc_ColorType
data ColorType = ColorGen | ColorSurf | ColorCurv deriving (Show, Eq, Enum)
