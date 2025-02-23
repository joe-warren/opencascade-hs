module Waterfall.Diagram
( Diagram
, LineType (..)
, Visibility (..)
, diagram
, diagramLines
) where

import qualified OpenCascade.TopoDS as TopoDS
import Data.Acquire (Acquire)
import Foreign.Ptr (Ptr)
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

data LineType = 
    OutLine
    | SharpLine
    | IsoLine
    | Rg1Line
    | RgNLine
    | OtherLine
    deriving (Eq, Ord, Show)

lineTypeToOpenCascade :: LineType -> HLRBRep.TypeOfResultingEdge
lineTypeToOpenCascade IsoLine = HLRBRep.IsoLine
lineTypeToOpenCascade OutLine = HLRBRep.OutLine
lineTypeToOpenCascade SharpLine = HLRBRep.Sharp
lineTypeToOpenCascade Rg1Line = HLRBRep.Rg1Line
lineTypeToOpenCascade RgNLine = HLRBRep.RgNLine
lineTypeToOpenCascade OtherLine = HLRBRep.Undefined

data Visibility = Visible | Hidden deriving (Eq, Ord, Show)

newtype Diagram = Diagram { runDiagram :: LineType -> Visibility -> Bool -> Acquire [Ptr TopoDS.Edge] }

instance Semigroup Diagram where
    a <> b = Diagram $ \lt v is3D -> (<>) <$> runDiagram a lt v is3D <*> runDiagram b lt v is3D

instance Monoid Diagram where
    mempty = Diagram $ \_ _ _ -> pure []

diagram :: V3 Double -> Solid -> Diagram
diagram projectionDirection solid = Diagram . unsafeFromAcquire $ do
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
        compoundOfEdges <- HLRBRep.HLRToShape.compoundOfEdges extractor (lineTypeToOpenCascade lt) (v == Visible) is3D
        rawEdges <- allEdges compoundOfEdges
        traverse buildEdgeCurve3D rawEdges

diagramLines :: LineType -> Visibility -> Diagram -> [Path2D]
diagramLines lt v d = unsafeFromAcquireT $ do 
    edges <- runDiagram d lt v False 
    wires <- traverse edgeToWire edges
    return $ (Path2D . ComplexRawPath) <$> wires

