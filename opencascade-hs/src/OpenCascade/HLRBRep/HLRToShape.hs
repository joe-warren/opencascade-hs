{-# LANGUAGE CApiFFI #-}
module OpenCascade.HLRBRep.HLRToShape
( HLRToShape
, fromAlgo
, compoundOfEdges
) where

import OpenCascade.HLRBRep.Types (Algo, HLRToShape)
import OpenCascade.HLRBRep.TypeOfResultingEdge (TypeOfResultingEdge)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.HLRBRep.Internal.Destructors (deleteHLRToShape)
import Foreign.Ptr 
import Data.Acquire (mkAcquire, Acquire)
import OpenCascade.Handle
import Foreign.C (CInt (..), CBool (..))
import OpenCascade.Internal.Bool (boolToCBool)

foreign import capi unsafe "hs_HLRBRep_HLRToShape.h hs_new_HLRBRep_HLRToShape_fromHandleAlgo" rawFromHandleAlgo :: Ptr (Handle Algo) -> IO (Ptr HLRToShape)

fromAlgo :: Ptr (Handle Algo) -> Acquire (Ptr HLRToShape)
fromAlgo algo = mkAcquire (rawFromHandleAlgo algo) deleteHLRToShape

foreign import capi unsafe "hs_HLRBRep_HLRToShape.h hs_HLRBRep_HLRToShape_compoundOfEdges" rawCompoundOfEdges :: Ptr HLRToShape -> CInt -> CBool -> CBool -> IO (Ptr TopoDS.Shape)

compoundOfEdges :: Ptr HLRToShape -> TypeOfResultingEdge -> Bool -> Bool -> Acquire (Ptr TopoDS.Shape)
compoundOfEdges hlrToShape typeOfEdge visible in3d =
    mkAcquire 
        (rawCompoundOfEdges hlrToShape (fromIntegral . fromEnum $ typeOfEdge) (boolToCBool visible) (boolToCBool in3d))
        deleteShape