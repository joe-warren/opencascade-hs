{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Waterfall.Diagram
( Diagram
, getDiagramFills
, getDiagramLines
, LineType (..)
, Visibility (..)
, solidDiagram
, pathDiagram
, shapeDiagram
, diagramLines
, diagramBoundingBox
) where

import Linear.V3 (V3)
import Linear.V2 (V2)
import Linear (_xy)
import Control.Lens ((^.))
import Waterfall.Internal.Solid (Solid (solidPaintMap), acquireSolid)
import qualified OpenCascade.HLRBRep.TypeOfResultingEdge as HLRBRep
import qualified OpenCascade.HLRBRep.Algo as HLRBRep.Algo
import qualified OpenCascade.HLRAlgo.Projector as HLRAlgo.Projector
import qualified OpenCascade.HLRBRep.HLRToShape as HLRBRep.HLRToShape
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Ax2 as GP.Ax2
import qualified OpenCascade.Bnd.Box as Bnd.Box
import qualified OpenCascade.BRepBndLib as BRepBndLib
import Waterfall.Internal.ToOpenCascade (v3ToDir, v3ToVertex)
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Control.Monad.IO.Class (liftIO)
import Waterfall.Internal.Finalizers (unsafeFromAcquire, toAcquire, fromAcquireT)
import Waterfall.Internal.Edges (allEdges, buildEdgeCurve3D, edgeToWire)
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.TwoD.Transforms (Transformable2D (..))
import Waterfall.Internal.FromOpenCascade (gpPntToV3)
import OpenCascade.Inheritance (upcast)
import Control.Monad (forM_, (<=<))
import Data.Map (Map)
import Waterfall.TwoD.Internal.Shape ( Shape )
import Waterfall.Paint (Paint)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Either (partitionEithers)
import Waterfall.Internal.Diagram (buildSolidDiagramFills)
import Waterfall.TwoD.Shape (shapePaths)

-- | `Diagram` of a Waterfall part
--
-- Contains:
--
-- * A collection of `Path2D` indexed by `LineType` and `Visibility`
-- * A collection of pairs of `Paint` and `Shape`, representing flat regions of colour
data Diagram = Diagram
     -- | The `Diagram`'s lines
     { getDiagramLines :: Map (LineType, Visibility) [Path2D]
     -- | Flat regions of colour
     , getDiagramFills :: [(Paint, Shape)]
     }

instance Semigroup Diagram where
    a <> b = Diagram 
        { getDiagramLines = M.unionWith (<>) (getDiagramLines a) (getDiagramLines b)
        , getDiagramFills = getDiagramFills a <> getDiagramFills b
        }

instance Monoid Diagram where
    mempty = Diagram mempty mempty

mapDiagram :: (forall a. Transformable2D a => a -> a) -> Diagram -> Diagram
mapDiagram f (Diagram theLines theFills) =
    Diagram 
        { getDiagramLines = fmap (fmap f) theLines
        , getDiagramFills = fmap (fmap f) theFills
        }

instance Transformable2D Diagram where
  matTransform2D m = mapDiagram (matTransform2D m)
  rotate2D theta = mapDiagram (rotate2D theta)
  scale2D s = mapDiagram (scale2D s)
  uScale2D s = mapDiagram (uScale2D s)
  translate2D v = mapDiagram (translate2D v)
  mirror2D v = mapDiagram (mirror2D v)

-- | Categorize the lines in a diagram
data LineType = 
    -- | Represents lines at the edge of objects, the "silhouette" 
    -- 
    -- Does not include those parts of the silhouette that are also sharp
    OutLine 
    -- | Sharp edges, parts of an object with C0 Continuity
    | SharpLine
    | RawLine HLRBRep.TypeOfResultingEdge
    deriving (Eq, Ord, Show)

normaliseLineType :: LineType -> LineType
normaliseLineType (RawLine HLRBRep.Sharp) = SharpLine
normaliseLineType (RawLine HLRBRep.OutLine) = OutLine
normaliseLineType other = other

lineTypeToOpenCascade :: LineType -> HLRBRep.TypeOfResultingEdge
lineTypeToOpenCascade OutLine = HLRBRep.OutLine
lineTypeToOpenCascade SharpLine = HLRBRep.Sharp
lineTypeToOpenCascade (RawLine lt) = lt

-- | Whether an edge is visible in a given projection, or not
data Visibility = Visible | Hidden deriving (Eq, Ord, Show)

-- | Produce a diagram of a `Solid`
-- 
-- Uses an orthographic projection, viewed from the provided direction
solidDiagram :: V3 Double -> Solid -> Diagram
solidDiagram projectionDirection solid =  unsafeFromAcquire $ do
    s' <- acquireSolid solid
    algo <- HLRBRep.Algo.new
    liftIO $ HLRBRep.Algo.add algo s'
    o <- GP.origin
    d <- v3ToDir projectionDirection
    projector <- HLRAlgo.Projector.fromAx2 =<< GP.Ax2.newAutoX o d
    liftIO $ do 
        HLRBRep.Algo.projector algo projector
        HLRBRep.Algo.update algo
        HLRBRep.Algo.hide algo

    extractor <- HLRBRep.HLRToShape.fromAlgo algo

    theLines <- fmap M.fromList . sequence $
        [ do
            compoundOfEdges <- HLRBRep.HLRToShape.compoundOfEdges 
                extractor 
                (lineTypeToOpenCascade lt) 
                (visibility == Visible) 
                False
            rawEdges <- allEdges compoundOfEdges
            wires <- liftIO . fromAcquireT $ traverse ( edgeToWire <=< buildEdgeCurve3D) rawEdges
            return ((lt, visibility), Path2D . ComplexRawPath <$> wires)

        | lt <- 
            [ OutLine
            , SharpLine
            , RawLine HLRBRep.IsoLine
            , RawLine HLRBRep.Rg1Line
            , RawLine HLRBRep.RgNLine
        ]
        , visibility <- [Visible, Hidden]
        ]

    theFills <-  buildSolidDiagramFills 
        (solidPaintMap solid) 
        s' 
        projectionDirection 
        projector
        extractor 

    return $ Diagram 
        { getDiagramLines = theLines
        , getDiagramFills = theFills
        }

-- | Produce a `Diagram` from a `Path2D`
-- 
-- @ diagramLines lt v . pathDiagram lt v = pure @
pathDiagram :: LineType -> Visibility -> Path2D -> Diagram
pathDiagram lt v path = Diagram
    { getDiagramLines = M.singleton (normaliseLineType lt, v) [path]
    , getDiagramFills = []
    }

-- | Produce a `Diagram` from a `Shape` and `Paint`
--
-- @ getDiagramFills . uncurry shapeDiagram = pure @
shapeDiagram :: Paint -> Shape -> Diagram
shapeDiagram paint shape = Diagram
    { getDiagramLines = mempty
    , getDiagramFills = [(paint, shape)]
    }

-- | Access the lines in a `Diagram` as `Path2D`
diagramLines :: LineType -> Visibility -> Diagram -> [Path2D]
diagramLines lt v = fromMaybe [] . M.lookup (normaliseLineType lt, v) . getDiagramLines

-- | Compute the Axis Aligned Bounding Box of a `Diagram`
-- 
-- Returns Nothing if the `Diagram` does not contain lines that are `OutLine` or `Sharp`
diagramBoundingBox :: Diagram -> Maybe (V2 Double, V2 Double)
diagramBoundingBox d = unsafeFromAcquire $ do
    let outline = diagramLines OutLine Visible d
    let sharpLine = diagramLines SharpLine Visible d
        fillLines = concatMap (shapePaths . snd) $ getDiagramFills d
    let allLines = outline <> sharpLine <> fillLines
    let partitionLine (Path2D (ComplexRawPath wire)) = Just (Left wire)
        partitionLine (Path2D (SinglePointRawPath point)) = Just (Right point)
        partitionLine _ = Nothing
        (allWires, allPoints) = partitionEithers . mapMaybe partitionLine $ allLines
    if null allWires && null allPoints
        then pure Nothing
        else do
            theBox <- Bnd.Box.new
            forM_ allWires $ \wire -> do
                w <- toAcquire wire
                liftIO $ BRepBndLib.addOptimal (upcast w) theBox True False
            forM_ allPoints $ \point -> do
                v <- v3ToVertex point
                liftIO $ BRepBndLib.addOptimal (upcast v) theBox True False

            p1 <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMin theBox
            p2 <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMax theBox
            return $ Just (p1 ^. _xy, p2 ^. _xy)

