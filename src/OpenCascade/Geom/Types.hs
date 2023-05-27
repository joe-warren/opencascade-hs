{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.Geom.Types
( Curve
, TrimmedCurve
, Surface
) where

import OpenCascade.Inheritance 

data Curve
data TrimmedCurve
data Surface

instance SubTypeOf Curve TrimmedCurve
