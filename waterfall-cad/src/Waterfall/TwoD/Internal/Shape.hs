{-# OPTIONS_HADDOCK not-home #-}
module Waterfall.TwoD.Internal.Shape
( Shape (..)
, acquireShape
, shapeFromAcquire
, union2D
, difference2D
, intersection2D
, unions2D
, intersections2D
, emptyShape
) where

import qualified OpenCascade.TopoDS as TopoDS
import Foreign.Ptr
import Data.Acquire (Acquire)
import Algebra.Lattice
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire)
import qualified OpenCascade.BRepAlgoAPI.Fuse as Fuse
import qualified OpenCascade.BRepAlgoAPI.Cut as Cut
import qualified OpenCascade.BRepAlgoAPI.Common as Common
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.BRepBuilderAPI.MakeFace as MakeFace
import qualified OpenCascade.BOPAlgo.Operation as BOPAlgo.Operation
import qualified OpenCascade.BOPAlgo.BOP as BOPAlgo.BOP
import qualified OpenCascade.BOPAlgo.Builder as BOPAlgo.Builder
import OpenCascade.Inheritance (upcast)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)

-- | A Region in 2D Space 
-- 
-- In general, this is used as a face, and extruded along some sort of path
--
-- Under the hood, this is represented by an OpenCascade `TopoDS.Shape`
-- 
-- This should be of type `TopoDS.Face`, constrained to the plane \( z=0 \).
--
-- Please feel free to report a bug if you're able to construct a `Shape`
-- which does not lie on this plane (without using Internal functions).
-- Or which is not either a `TopoDS.Face`, or a composite of faces.
newtype Shape = Shape { rawShape :: Ptr TopoDS.Shape }

acquireShape :: Shape -> Acquire (Ptr TopoDS.Shape)
acquireShape (Shape ptr) = toAcquire ptr

shapeFromAcquire :: Acquire (Ptr TopoDS.Shape) -> Shape
shapeFromAcquire = Shape . unsafeFromAcquire

toBoolean2D :: (Ptr TopoDS.Shape.Shape -> Ptr TopoDS.Shape.Shape -> Acquire (Ptr TopoDS.Shape.Shape)) -> Shape -> Shape -> Shape
toBoolean2D f (Shape ptrA) (Shape ptrB) = Shape . unsafeFromAcquire $ do
    a <- toAcquire ptrA
    b <- toAcquire ptrB
    f a b

-- | Take the union of two 2D shapes.
-- The region occupied by either one of them
union2D :: Shape -> Shape -> Shape
union2D = toBoolean2D Fuse.fuse

-- | Take the difference of two 2D shapes.
-- The region occupied by the first, but not the second
difference2D :: Shape -> Shape -> Shape
difference2D = toBoolean2D Cut.cut

-- | Take the intersection of two 2D shapes.
-- The region occupied by both of them
intersection2D :: Shape -> Shape -> Shape
intersection2D = toBoolean2D Common.common

toBooleans2D :: BOPAlgo.Operation.Operation -> [Shape] -> Shape
toBooleans2D _ [] = emptyShape
toBooleans2D _ [x] = x
toBooleans2D op (h:shapes) = Shape . unsafeFromAcquire $ do
    firstPtr <- toAcquire . rawShape $ h
    ptrs <- traverse (toAcquire . rawShape) shapes
    bop <- BOPAlgo.BOP.new
    let builder = upcast bop
    liftIO $ do
        BOPAlgo.BOP.setOperation bop op
        BOPAlgo.Builder.addArgument builder firstPtr
        traverse_ (BOPAlgo.BOP.addTool bop) ptrs
        BOPAlgo.Builder.setRunParallel builder True
        BOPAlgo.Builder.perform builder
    BOPAlgo.Builder.shape builder

-- | Take the union of a list of 2D shapes
-- May be more performant than chaining multiple applications of `union2D`
unions2D :: [Shape] -> Shape
unions2D = toBooleans2D BOPAlgo.Operation.Fuse

-- | Take the intersection of a list of 2D shapes
-- May be more performant than chaining multiple applications of `intersection2D`
intersections2D :: [Shape] -> Shape
intersections2D = toBooleans2D BOPAlgo.Operation.Common

-- | An empty 2D shape
emptyShape :: Shape
emptyShape = Shape . unsafeFromAcquire $ 
    upcast <$> (MakeFace.face =<< MakeFace.new)

-- defining these boolean operators here, rather than in Waterfall.TwoD.Booleans 
-- means that we can use them in typeclass instances without resorting to orphans

instance Semigroup Shape where
    (<>) = union2D

instance Monoid Shape where
    mempty = emptyShape
    mconcat = unions2D

instance Lattice Shape where 
    (/\) = intersection2D
    (\/) = union2D

instance BoundedJoinSemiLattice Shape where
    bottom = emptyShape