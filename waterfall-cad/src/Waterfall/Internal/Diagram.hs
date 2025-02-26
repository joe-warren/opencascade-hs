{-# OPTIONS_HADDOCK not-home #-}
module Waterfall.Internal.Diagram 
( RawDiagram (..)
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

newtype RawDiagram = RawDiagram { runDiagram :: HLRBRep.TypeOfResultingEdge -> Bool -> Bool -> Acquire [Ptr TopoDS.Edge] }

instance Semigroup RawDiagram where
    a <> b = RawDiagram $ \lt v is3D -> (<>) <$> runDiagram a lt v is3D <*> runDiagram b lt v is3D

instance Monoid RawDiagram where
    mempty = RawDiagram $ \_ _ _ -> pure []