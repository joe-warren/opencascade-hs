{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.Geom.Types
( Curve
, TrimmedCurve
, BezierCurve
, BSplineCurve
, Surface
) where

import OpenCascade.Inheritance 

data Curve
data TrimmedCurve
data BezierCurve
data BSplineCurve
data Surface

instance SubTypeOf Curve TrimmedCurve
instance SubTypeOf Curve BezierCurve
instance SubTypeOf Curve BSplineCurve
