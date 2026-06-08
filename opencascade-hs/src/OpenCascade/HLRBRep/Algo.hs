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
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr
import Data.Acquire (mkAcquire, Acquire)
import OpenCascade.Handle

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_new_HLRBRep_Algo" rawNew :: IO (Ptr (Handle Algo))

new :: Acquire (Ptr (Handle Algo))
new = mkAcquire rawNew deleteAlgo

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_projector" projector :: Ptr (Handle Algo) -> Ptr HLRAlgo.Projector -> IO ()

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_add" rawAdd
    :: Ptr (Handle Algo)
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

add :: Ptr (Handle Algo) -> Ptr TopoDS.Shape -> IO ()
add algo shape = wrapException $ rawAdd algo shape

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_update" rawUpdate
    :: Ptr (Handle Algo)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

update :: Ptr (Handle Algo) -> IO ()
update algo = wrapException $ rawUpdate algo

foreign import capi unsafe "hs_HLRBRep_Algo.h hs_HLRBRep_Algo_hide" rawHide
    :: Ptr (Handle Algo)
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

hide :: Ptr (Handle Algo) -> IO ()
hide algo = wrapException $ rawHide algo
