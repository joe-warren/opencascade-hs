module OpenCascade.GeomAbs.Shape 
( Shape (..)
) where 

-- This needs to stay up to date with GeomAbs_Shape.hxx
data Shape = C0 | G1 | C1 | G2 | C2 | C3 | CN 
    deriving (Enum, Show)