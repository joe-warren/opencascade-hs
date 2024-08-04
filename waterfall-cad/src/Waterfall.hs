{-| 
Module: Waterfall

This module simply re-exports everything from the various modules
that make up the waterfall-cad package.
-}
module Waterfall 
(
-- | `Solid` is the main data type in the Waterfall-CAD API,
-- The "Waterfall.Solids" module is the easiest way to construct 
-- instances of `Solid`.
  module Waterfall.Solids
-- | The functions in this module can be used to transform `Solid`s.
, module Waterfall.Transforms
-- | `Solid`'s can be combined together with 
-- [Constructive Solid Geometry](https://en.wikipedia.org/wiki/Constructive_solid_geometry).
, module Waterfall.Booleans
, module Waterfall.Booleans.Operators
-- | Once you've generated a `Solid`, 
-- the functions in `Waterfall.IO` can be used to save it.
-- 
-- The `Waterfall.IO` module also supports reading `Solid`s from a variety of file formats.
, module Waterfall.IO
-- | Calculating Axis Aligned Bounding Boxes from a `Solid`.
, module Waterfall.BoundingBox.AxisAligned
-- | Calculating Oriented Bounding Boxes from a `Solid`.
, module Waterfall.BoundingBox.Oriented
-- |  This module deals with adding rounds\/fillets\/bevels to a `Solid`.
, module Waterfall.Fillet
-- |  This module deals with offsetting a `Solid`: either shrinking/expanding it.
, module Waterfall.Offset
-- | Paths in 3D space.
, module Waterfall.Path
-- | Two Dimensional Shapes.
, module Waterfall.TwoD.Shape
-- | Sweep a 2D `Shape` along a `Path`, constructing a `Solid`.
, module Waterfall.Sweep
-- | Generate a [Loft](https://en.wikipedia.org/wiki/Loft_\(3D\)) between a sequence of `Path`s
, module Waterfall.Loft
-- | Construct a `Solid` of revolution from a `Path2D`.
, module Waterfall.Revolution
-- | Transforms for data types that exist in Two Dimensional space, like `Shape` and `Path2D`.
, module Waterfall.TwoD.Transforms
-- | Paths in 2D space, can be built up into a `Shape`.
, module Waterfall.TwoD.Path2D
-- | Create a `Shape` from text, rendered using a specific `Font`.
, module Waterfall.TwoD.Text
-- | Paths in 2D / 3D space.
--
-- This module defines functions that can be used with "Waterfall.Path" or "Waterfall.TwoD.Path2D".
-- Those modules both export monomorphized variants of the functions defined in this module.
, module Waterfall.Path.Common
)where

import Waterfall.Booleans
import Waterfall.Booleans.Operators
import Waterfall.BoundingBox.AxisAligned
import Waterfall.BoundingBox.Oriented
import Waterfall.Fillet
import Waterfall.IO
import Waterfall.Offset
import Waterfall.Path.Common
import Waterfall.Path
import Waterfall.Revolution
import Waterfall.Solids
import Waterfall.Sweep
import Waterfall.Loft
import Waterfall.Transforms
import Waterfall.TwoD.Transforms
import Waterfall.TwoD.Path2D
import Waterfall.TwoD.Shape
import Waterfall.TwoD.Text