{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Waterfall.TwoD.Internal.Path2D
( Path2D (..)
) where

import Waterfall.Internal.Path.Common (RawPath (..))

-- | A Path in 2D Space 
--
-- Under the hood, this is represented by an OpenCascade `TopoDS.Wire`, constrained to the plane \(z=0\).
--
-- Please feel free to report a bug if you're able to construct a `Path2D`
-- which does not lie on this plane (without using Internal functions).
--
-- The monoid instance  Joins `Path2D`s, @ a <> b @ connects the end point of @ a @ to the start of @ b @, if these points are not coincident, a line is created between them.
newtype Path2D = Path2D { rawPath :: RawPath } deriving (Semigroup, Monoid) via RawPath