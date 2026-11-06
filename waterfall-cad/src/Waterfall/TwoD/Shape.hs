module Waterfall.TwoD.Shape
( Shape
, makeShape
, shapePaths
, unitCircle
, unitSquare
, centeredSquare
, unitPolygon
, looseShapes
, section
, silhouette
-- * Boolean operations
, union2D
, difference2D
, intersection2D
, unions2D
, intersections2D
, emptyShape
, area
, centroid
) where

import Waterfall.TwoD.Internal.Shape (Shape (..), union2D, difference2D, intersection2D, unions2D, intersections2D, emptyShape)
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Waterfall.TwoD.Transforms (translate2D, rotate2D)
import Waterfall.Internal.Finalizers (acquireForeignPtr, unsafeFromAcquirePtr, unsafeFromAcquirePtrT, unsafeFromAcquire, NoPtr)
import Waterfall.Internal.Edges (allWires, allSubShapesWithCopy)
import qualified OpenCascade.BRepBuilderAPI.MakeFace as MakeFace
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.GP.Pln as GP.Pln
import OpenCascade.Inheritance (upcast)
import Linear (unit, _x, _y, _xy, zero, V2 (..))
import Waterfall.Path.Common (pathFrom, arcViaTo, lineTo, line, closeLoop)
import Waterfall.Internal.Path.Common (RawPath(ComplexRawPath))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (filterM, (<=<))
import Waterfall.Internal.Solid (Solid, acquireSolid)
import Waterfall.Internal.Diagram (silhouetteShape)
import qualified OpenCascade.BRepAlgoAPI.Common as Common
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import Control.DeepSeq (NFData)
import Foreign (Ptr)
import Data.Acquire (Acquire)
import Control.Lens ((^.))
import qualified OpenCascade.GProp.GProps as GProps
import qualified OpenCascade.BRepGProp as BRepGProp
import Waterfall.Internal.FromOpenCascade (gpPntToV3)

-- | Construct a 2D Shape from a path 
--
-- Closes it (with a straight line) if it's open
makeShape :: Path2D -> Shape
makeShape path = case closeLoop path of 
    (Path2D (ComplexRawPath r)) -> Shape . unsafeFromAcquirePtr  $ do
        p <- acquireForeignPtr r
        upcast <$> (MakeFace.face =<< MakeFace.fromWire p False)
    _ -> Shape . unsafeFromAcquirePtr $
        upcast <$> (MakeFace.face =<< MakeFace.new)

-- | Get the paths back from a 2D shape
-- 
-- Ideally:
--
-- @
-- shapePaths . makeShape ≡ pure
-- @
-- 
-- Although this can only hold when the shape has one boundary path, 
-- and is not guaranteed
shapePaths :: Shape -> [Path2D] 
shapePaths (Shape r) = fmap (Path2D . ComplexRawPath) . unsafeFromAcquirePtrT $ do
    s <- acquireForeignPtr r 
    allWires s 

-- | Circle with radius 1, centered on the origin
unitCircle :: Shape
unitCircle = makeShape $ pathFrom (unit _x)
                [ arcViaTo (unit _y) (negate $ unit _x)
                , arcViaTo (negate $ unit _y) (unit _x)
                ]

-- | Square with side length of 1, one vertex on the origin, another on \( (1, 1) \)
unitSquare :: Shape
unitSquare =
    makeShape $ pathFrom zero
        [ lineTo (unit _x)
        , lineTo (V2 1 1)
        , lineTo (unit _y)
        , lineTo zero
        ]

-- | Square with side length of 1, centered on the origin
centeredSquare :: Shape
centeredSquare = translate2D (V2 (-0.5) (-0.5)) unitSquare

-- | \(n\) sided Polygon, centered on the origin
-- 
-- Empty when n <= 2
unitPolygon :: Integer -> Shape
unitPolygon n 
    | n <= 2 = mempty
    | otherwise = 
        let n' = fromIntegral n
            points = [
                rotate2D (2 * pi * fromIntegral i / n') (unit _x)
                | i <- [0..n]
                ]
            paths = mconcat [
                line a b
                | (a, b) <- zip points (drop 1 points)
                ]
        in makeShape paths

-- | Split a `Shape` into a list of disjoint parts
looseShapes :: Shape -> [Shape] 
looseShapes (Shape r) = fmap Shape . unsafeFromAcquirePtrT $ do
    shape <- acquireForeignPtr r
    filterM (fmap (>0) . liftIO . TopoDS.Shape.nbChildren)
        =<< allSubShapesWithCopy ShapeEnum.Face shape

-- | Compute the cross-section of a `Solid` on the @xy@ plane
--
-- The cross section is computed at @z=0@
section :: Solid -> Shape
section solid = Shape . unsafeFromAcquirePtr $ do
    s <- acquireSolid solid
    face <- MakeFace.face =<< MakeFace.fromPln =<< GP.Pln.new
    MakeShape.shape . upcast =<< Common.fromShapes s (upcast face)

-- | Compute the Shadow of a `Solid` projected onto the @xy@ plane
silhouette :: Solid -> Shape
silhouette solid = unsafeFromAcquire $ do
    s <- acquireSolid solid
    mconcat <$> silhouetteShape s 


gPropQuery :: (NoPtr a, NFData a) => (Ptr GProps.GProps -> Acquire a) -> Shape -> a
gPropQuery f (Shape s) = unsafeFromAcquire $ do
    shape <- acquireForeignPtr s
    gProp <- GProps.new
    liftIO $ BRepGProp.surfaceProperties shape gProp False False
    f gProp

-- | Area of the `Shape`
area :: Shape -> Double
area = gPropQuery (liftIO . GProps.mass)

-- | Center Of Mass of the `Shape` 
-- 
-- The 2D counterpart of `Waterfall.Solids.centerOfMass`
centroid :: Shape -> V2 Double 
centroid = (^. _xy) . (gPropQuery ((liftIO . gpPntToV3) <=< GProps.centreOfMass))