module OpenCascade.Graphic3D.VerticalTextAlignment
( VerticalTextAlignment (..)
) where

-- Should match the order in Graphic3D.HorizontalTextAlignment.hxx
data VerticalTextAlignment = Bottom | Center | Top | TopFirstLine deriving (Enum, Show)