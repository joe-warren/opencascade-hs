module OpenCascade.GeomAbs.CurveType
( CurveType (..)
) where

-- This needs to stay up to date with GeomAbs_CurveType.hxx
data CurveType
    = Line 
    | Circle
    | Ellipse
    | Hyperbola 
    | Parabola 
    | BezierCurve
    | BSplineCurve
    | OffsetCurve
    | OtherCurve
    deriving (Enum, Show)