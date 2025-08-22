{-|
[Constructive Solid Geometry \(CSG\)](https://en.wikipedia.org/wiki/Constructive_solid_geometry) operations on 2D `Shape`.
-}
module Waterfall.TwoD.Booleans
( union2D
, difference2D
, intersection2D
, unions2D
, intersections2D
, emptyShape
) where

import Waterfall.TwoD.Internal.Shape (union2D, unions2D, difference2D, intersection2D, intersections2D, emptyShape)
