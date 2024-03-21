module OpenCascade.TopAbs.Orientation 
( Orientation (..)
) where

-- this should match the orientation in TopAbs_Orientation
data Orientation = Forward | Reversed | Internal | External deriving (Show, Enum, Eq)
