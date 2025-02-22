module OpenCascade.HLRBRep.TypeOfResultingEdge
( TypeOfResultingEdge (..)
) where 

data TypeOfResultingEdge 
    = Undefined
    | IsoLine
    | OutLine
    | Rg1Line
    | RgNLine
    | Sharp
    deriving (Eq, Enum, Show)