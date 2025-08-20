module Waterfall.TwoD.Booleans
( union2D
, difference2D
, intersection2D
, unions2D
, intersections2D
, complement2D
, nowhere2D
) where

import Waterfall.TwoD.Internal.Shape (Shape (..))
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
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire)

-- | Helper function for binary boolean operations on 2D shapes
toBoolean2D :: (Ptr TopoDS.Shape.Shape -> Ptr TopoDS.Shape.Shape -> Acquire (Ptr TopoDS.Shape.Shape)) -> Shape -> Shape -> Shape
toBoolean2D f (Shape ptrA) (Shape ptrB) = Shape . unsafeFromAcquire $ do
    a <- toAcquire ptrA
    b <- toAcquire ptrB
    f a b

-- | Take the union of two 2D shapes
-- The region occupied by either one of them
union2D :: Shape -> Shape -> Shape
union2D = toBoolean2D Fuse.fuse

-- | Take the difference of two 2D shapes
-- The region occupied by the first, but not the second
difference2D :: Shape -> Shape -> Shape
difference2D = toBoolean2D Cut.cut

-- | Take the intersection of two 2D shapes
-- The region occupied by both of them
intersection2D :: Shape -> Shape -> Shape
intersection2D = toBoolean2D Common.common

-- | Helper function for multi-shape boolean operations
toBooleans2D :: BOPAlgo.Operation.Operation -> [Shape] -> Shape
toBooleans2D _ [] = nowhere2D
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

-- | Invert a 2D shape (complement)
-- The complement of a shape represents the shape with the same boundary,
-- but where the opposite side is the "inside" of the shape
complement2D :: Shape -> Shape
complement2D (Shape ptr) = Shape . unsafeFromAcquire $ 
    TopoDS.Shape.complemented =<< toAcquire ptr

-- | An empty 2D shape
nowhere2D :: Shape
nowhere2D = Shape . unsafeFromAcquire $ 
    upcast <$> (MakeFace.face =<< MakeFace.new)
