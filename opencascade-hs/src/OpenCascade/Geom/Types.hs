{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.Geom.Types
( Curve
, TrimmedCurve
, BezierCurve
, Surface
) where

import OpenCascade.Inheritance 

data Curve
data TrimmedCurve
data BezierCurve
data Surface

instance SubTypeOf Curve TrimmedCurve
instance SubTypeOf Curve BezierCurve
