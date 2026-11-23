{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpenCascade.ShapeUpgrade.Types
( UnifySameDomain
, ShapeDivide
, ShapeDivideContinuity
) where

import OpenCascade.Inheritance (SubTypeOf)

data UnifySameDomain
data ShapeDivide
data ShapeDivideContinuity

instance SubTypeOf ShapeDivide ShapeDivideContinuity
