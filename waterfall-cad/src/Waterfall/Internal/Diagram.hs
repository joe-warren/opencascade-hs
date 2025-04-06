{-# OPTIONS_HADDOCK not-home #-}
module Waterfall.Internal.Diagram 
( RawDiagram (..)
) where

import qualified OpenCascade.TopoDS as TopoDS
import Data.Acquire (Acquire)
import Foreign.Ptr (Ptr)
import qualified OpenCascade.HLRBRep.TypeOfResultingEdge as HLRBRep

newtype RawDiagram = RawDiagram { runDiagram :: HLRBRep.TypeOfResultingEdge -> Bool -> Bool -> Acquire [Ptr TopoDS.Edge] }

instance Semigroup RawDiagram where
    a <> b = RawDiagram $ \lt v is3D -> (<>) <$> runDiagram a lt v is3D <*> runDiagram b lt v is3D

instance Monoid RawDiagram where
    mempty = RawDiagram $ \_ _ _ -> pure []