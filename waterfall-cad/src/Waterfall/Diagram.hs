{-# LANGUAGE DerivingVia #-}
module Waterfall.Diagram
( Diagram
, LineType (..)
, Visibility (..)
, diagram
, diagramLines
) where

import Linear.V3 (V3)
import Waterfall.Internal.Solid (Solid (), acquireSolid)
import qualified OpenCascade.HLRBRep.TypeOfResultingEdge as HLRBRep
import qualified OpenCascade.HLRBRep.Algo as HLRBRep.Algo
import qualified OpenCascade.HLRAlgo.Projector as HLRAlgo.Projector
import qualified OpenCascade.HLRBRep.HLRToShape as HLRBRep.HLRToShape
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Ax2 as GP.Ax2
import Waterfall.Internal.ToOpenCascade (v3ToDir)
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Control.Monad.IO.Class (liftIO)
import Waterfall.Internal.Finalizers (unsafeFromAcquire, unsafeFromAcquireT)
import Waterfall.Internal.Edges (allEdges, buildEdgeCurve3D, edgeToWire)
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.Internal.Diagram (RawDiagram (..))
import Waterfall.TwoD.Transforms (Transformable2D)

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

