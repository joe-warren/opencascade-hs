module OpenCascade.Quantity.TypeOfColor
( TypeOfColor (..)
) where

-- this should match the enumeration in Quantity_TypeOfColor
data TypeOfColor = RGB | SRGB | HLS | CIELab | CIELch deriving (Show, Eq, Enum)
