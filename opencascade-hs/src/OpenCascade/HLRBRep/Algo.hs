{-# LANGUAGE CApiFFI #-}
module OpenCascade.HLRBRep.Algo
( Algo
, new
, toHandle
, projector
, update
, hide
, add
) where
import OpenCascade.HLRBRep.Types (Algo)
import qualified OpenCascade.HLRAlgo.Types as HLRAlgo
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.HLRBRep.Internal.Destructors (deleteAlgo, deleteHandleAlgo)
import Foreign.Ptr 
import Data.Acquire (mkAcquire, Acquire)
import OpenCascade.Handle

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_new_HLRBRep_Algo" rawNew :: IO (Ptr Algo)

new :: Acquire (Ptr Algo)
new = mkAcquire rawNew deleteAlgo


foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_toHandle" rawToHandle :: Ptr Algo -> IO (Ptr (Handle Algo))

toHandle :: Ptr Algo -> Acquire (Ptr (Handle Algo))
toHandle algo = mkAcquire (rawToHandle algo) deleteHandleAlgo

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_projector" projector :: Ptr Algo -> Ptr HLRAlgo.Projector -> IO ()

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_add" add :: Ptr Algo -> Ptr TopoDS.Shape -> IO ()

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_update" update :: Ptr Algo -> IO ()

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_hide" hide :: Ptr Algo -> IO ()
