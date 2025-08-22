{-|
[Constructive Solid Geometry \(CSG\)](https://en.wikipedia.org/wiki/Constructive_solid_geometry) operations on `Solid`.

This module also provides the 'Boolean' typeclass for unified boolean operations
that work on both 2D shapes and 3D solids.
-}
module Waterfall.Booleans
( union3D
, difference3D
, intersection3D
, complement3D
, unions3D
, intersections3D
, Boolean(..)
) where

import Waterfall.Internal.Solid(union3D, unions3D, difference3D, intersection3D, intersections3D, complement3D)
import qualified Waterfall.Solids as Solids
import qualified Waterfall.TwoD.Internal.Shape as Shape
import Waterfall.TwoD.Internal.Shape (union2D, difference2D, intersection2D, unions2D, intersections2D)

-- | Typeclass for boolean operations on geometric objects.
class Boolean a where
    -- | Take the union of two objects
    -- 
    -- The region occupied by either one of them.
    union :: a -> a -> a
    
    -- | Take the difference of two objects
    -- 
    -- The region occupied by the first, but not the second.
    difference :: a -> a -> a
    
    -- | Take the intersection of two objects
    --
    -- The region occupied by both of them.
    intersection :: a -> a -> a
    
    -- | The empty object (identity for union, annihilator for intersection)
    --
    -- For union: @empty `union` x = x `union` empty = x@
    -- For intersection: @empty `intersection` x = x `intersection` empty = empty@
    empty :: a
    
    -- | Take the union of a list of objects
    --
    -- May be more performant than chaining multiple applications of `union`.
    unions :: [a] -> a
    unions = foldr union empty
    
    -- | Take the intersection of a list of objects
    --
    -- May be more performant than chaining multiple applications of `intersection`.
    intersections :: [a] -> a
    intersections [] = empty
    intersections xs = foldr1 intersection xs

instance Boolean Solids.Solid where
    union = union3D
    difference = difference3D
    intersection = intersection3D
    empty = Solids.emptySolid
    unions = unions3D
    intersections = intersections3D

instance Boolean Shape.Shape where
    union = union2D
    difference = difference2D
    intersection = intersection2D
    empty = Shape.emptyShape
    unions = unions2D
    intersections = intersections2D