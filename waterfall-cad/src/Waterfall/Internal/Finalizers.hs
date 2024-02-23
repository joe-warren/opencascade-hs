module Waterfall.Internal.Finalizers 
( unsafeFromAcquire
, toAcquire
) where

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans.Resource (runResourceT, unprotect)
import Data.Acquire (Acquire, mkAcquire, allocateAcquire)
import System.Mem.Weak (addFinalizer)
import Control.Monad.Primitive (touch)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

unsafeFromAcquire :: Acquire a -> a 
unsafeFromAcquire a = unsafePerformIO . runResourceT $ do
    (releaseKey, v) <- allocateAcquire a
    release <- fromMaybe (pure ()) <$> unprotect releaseKey
    liftIO $ addFinalizer v release
    return v

toAcquire :: a -> Acquire a
toAcquire value = mkAcquire (pure value) (touch) 