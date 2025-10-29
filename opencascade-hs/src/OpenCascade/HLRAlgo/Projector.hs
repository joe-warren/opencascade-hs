{-# LANGUAGE CApiFFI #-}
module OpenCascade.HLRAlgo.Projector
( Projector
, fromAx2
) where
import qualified OpenCascade.GP.Types as GP
import OpenCascade.HLRAlgo.Types (Projector)
import OpenCascade.HLRAlgo.Internal.Destructors (deleteProjector)
import Foreign.Ptr 
import Data.Acquire (mkAcquire, Acquire)

foreign import capi unsafe "hs_HLRAlgo_Projector.h hs_new_HLRAlgo_Projector_fromAx2" rawFromAx2 :: Ptr GP.Ax2 -> IO (Ptr Projector)

fromAx2 :: Ptr GP.Ax2 -> Acquire (Ptr Projector)
fromAx2 ax2 = mkAcquire (rawFromAx2 ax2) deleteProjector

