{-# LANGUAGE CApiFFI #-}
module OpenCascade.HLRBRep.Algo
( Algo
, new
, projector
, update
, hide
, add
) where
import OpenCascade.HLRBRep.Types (Algo)
import qualified OpenCascade.HLRAlgo.Types as HLRAlgo
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.HLRBRep.Internal.Destructors (deleteAlgo)
import Foreign.Ptr 
import Data.Acquire (mkAcquire, Acquire)
import OpenCascade.Handle

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_new_HLRBRep_Algo" rawNew :: IO (Ptr (Handle Algo))

new :: Acquire (Ptr (Handle Algo))
new = mkAcquire rawNew deleteAlgo

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_projector" projector :: Ptr (Handle Algo) -> Ptr HLRAlgo.Projector -> IO ()

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_add" add :: Ptr (Handle Algo) -> Ptr TopoDS.Shape -> IO ()

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_update" update :: Ptr (Handle Algo) -> IO ()

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_hide" hide :: Ptr (Handle Algo) -> IO ()
