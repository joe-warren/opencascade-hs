{-|
[Constructive Solid Geometry \(CSG\)](https://en.wikipedia.org/wiki/Constructive_solid_geometry) operations on `Solid`.
-}
module Waterfall.Booleans
( union
, difference
, intersection
, complement
, unions
, intersections
) where

import Waterfall.Internal.Solid(union, unions, difference, intersection, intersections, complement)