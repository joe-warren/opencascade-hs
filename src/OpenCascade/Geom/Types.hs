{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.Geom.Types
( Curve
, TrimmedCurve
) where

import OpenCascade.Inheritance 

data Curve
data TrimmedCurve

instance SubTypeOf Curve TrimmedCurve
