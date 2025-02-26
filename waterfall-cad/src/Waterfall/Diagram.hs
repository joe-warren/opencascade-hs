{-# LANGUAGE DerivingVia #-}
module Waterfall.Diagram
( Diagram
, LineType (..)
, Visibility (..)
, diagram
, diagramLines
, diagramBoundingBox
) where

import Linear.V3 (V3)
import Linear.V2 (V2)
import Linear (_xy)
import Control.Lens ((^.))
import Waterfall.Internal.Solid (Solid (), acquireSolid)
import qualified OpenCascade.HLRBRep.TypeOfResultingEdge as HLRBRep
import qualified OpenCascade.HLRBRep.Algo as HLRBRep.Algo
import qualified OpenCascade.HLRAlgo.Projector as HLRAlgo.Projector
import qualified OpenCascade.HLRBRep.HLRToShape as HLRBRep.HLRToShape
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Ax2 as GP.Ax2
import qualified OpenCascade.Bnd.Box as Bnd.Box
import qualified OpenCascade.BRepBndLib as BRepBndLib
import Waterfall.Internal.ToOpenCascade (v3ToDir)
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Control.Monad.IO.Class (liftIO)
import Waterfall.Internal.Finalizers (unsafeFromAcquire, unsafeFromAcquireT)
import Waterfall.Internal.Edges (allEdges, buildEdgeCurve3D, edgeToWire)
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.Internal.Diagram (RawDiagram (..))
import Waterfall.TwoD.Transforms (Transformable2D)
import Waterfall.Internal.FromOpenCascade (gpPntToV3)
import OpenCascade.Inheritance (upcast)
import Control.Monad (forM_)

newtype Diagram = Diagram { rawDiagram :: RawDiagram }
    deriving (Semigroup, Monoid, Transformable2D) via RawDiagram

data LineType = 
    OutLine
    | SharpLine
    | RawLine HLRBRep.TypeOfResultingEdge
    deriving (Eq, Ord, Show)

lineTypeToOpenCascade :: LineType -> HLRBRep.TypeOfResultingEdge
lineTypeToOpenCascade OutLine = HLRBRep.OutLine
lineTypeToOpenCascade SharpLine = HLRBRep.Sharp
lineTypeToOpenCascade (RawLine lt) = lt

data Visibility = Visible | Hidden deriving (Eq, Ord, Show)

diagram :: V3 Double -> Solid -> Diagram
diagram projectionDirection solid = Diagram . RawDiagram . unsafeFromAcquire $ do
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

    handleAlgo <- HLRBRep.Algo.toHandle algo
    extractor <- HLRBRep.HLRToShape.fromHandleAlgo handleAlgo

    return $ \lt v is3D -> do
        compoundOfEdges <- HLRBRep.HLRToShape.compoundOfEdges extractor lt v is3D
        rawEdges <- allEdges compoundOfEdges
        traverse buildEdgeCurve3D rawEdges

diagramLines :: LineType -> Visibility -> Diagram -> [Path2D]
diagramLines lt v d = unsafeFromAcquireT $ do 
    edges <- runDiagram (rawDiagram d) (lineTypeToOpenCascade lt) (v == Visible) False 
    wires <- traverse edgeToWire edges
    return $ (Path2D . ComplexRawPath) <$> wires

diagramBoundingBox :: Diagram -> Maybe (V2 Double, V2 Double)
diagramBoundingBox d = unsafeFromAcquire $ do
    outline <- runDiagram (rawDiagram d) HLRBRep.OutLine True False
    if null outline
        then pure Nothing
        else do
            theBox <- Bnd.Box.new
            forM_ outline $ \s -> (liftIO $ BRepBndLib.addOptimal (upcast s) theBox True False)
            p1 <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMin theBox
            p2 <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMax theBox
            return $ Just (p1 ^. _xy, p2 ^. _xy)

