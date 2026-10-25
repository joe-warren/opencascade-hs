{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, PolyKinds, UndecidableInstances, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-| 
Module: Waterfall.Internal.Finalizers

These functions exist because the underlying @opencascade-hs@ library, 
makes heavy use of @Data.Acquire@ from @resourcet@ to handle memory management.

However @waterfall-cad@ does not (at the highest level) keep values in the @Acquire@ monad. 

(This is required to support functions like `Waterfall.Solids.volume`, which return pure Haskell primitives.)

-}
module Waterfall.Internal.Finalizers 
( -- * Pointer valued resources 
 unsafeFromAcquirePtr
, unsafeFromAcquirePtrWithCatch
, unsafeFromAcquirePtrT
, unsafeFromAcquirePtrTWithCatch
, fromAcquirePtr
, fromAcquirePtrT
, acquireForeignPtr
-- * Pure valued resources
, runAcquire
, unsafeFromAcquire
, unsafeFromAcquireWithCatch
-- * Type Family
, NoPtr
) where

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans.Resource (runResourceT, unprotect)
import Data.Acquire (Acquire, mkAcquire, allocateAcquire, with)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Control.Monad (forM, when)
import Data.IORef (newIORef, atomicModifyIORef)
import Control.Exception (try, evaluate)
import Control.Arrow (left)
import Waterfall.Error (WaterfallError (..))
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import qualified Foreign.Concurrent as Concurrent
import Control.DeepSeq (NFData, force)
import Data.Kind (Constraint)
import GHC.TypeLits (TypeError, ErrorMessage (..))


-- | Convert a resource in the @Data.Acquire.Acquire@ monad to a @ForeignPtr@ in @IO@
--
-- the @free@ action of the resource is called when the @ForeignPtr@ is garbage collected,
-- so may run at an unpredictable time.
fromAcquirePtr :: Acquire (Ptr a) -> IO (ForeignPtr a)
fromAcquirePtr a = runResourceT $ do
    (releaseKey, ptr) <- allocateAcquire a
    release <- fromMaybe (pure ()) <$> unprotect releaseKey
    liftIO $ Concurrent.newForeignPtr ptr release
    
-- | variant of `fromAcquirePtr` for a resource of a container of pointers
-- 
-- the free action of the resource is called once every @ForeignPtr@ in the container has been garbage collected
fromAcquirePtrT :: Traversable f => Acquire (f (Ptr a)) -> IO (f (ForeignPtr a)) 
fromAcquirePtrT a = runResourceT $ do
    (releaseKey, ptrs) <- allocateAcquire a
    release <- fromMaybe (pure ()) <$> unprotect releaseKey
    ref <- liftIO $ newIORef (length ptrs)
    let finalize = do
            isLast <- atomicModifyIORef ref (\count -> (count - 1, count == 1))
            when isLast release
    liftIO $ when (null ptrs) release
    forM ptrs $ \ptr -> do 
        liftIO $ Concurrent.newForeignPtr ptr finalize

-- | Converting to a value in the `Data.Acquire.Acquire` monad, to a raw value.
-- Analagous to calling `unsafePerformIO` to extract a value in the `IO` monad.
-- The same constraints as apply to `unsafePerformIO` apply to this method. 
-- That is, it should only be used on "philosophically pure" actions.
--
-- The `free` action of the resource is called when the underlying value goes out of scope of the Haskell garbage collection,
-- so may run at an unpredictable time.
{-# NOINLINE unsafeFromAcquirePtr #-}
unsafeFromAcquirePtr :: Acquire (Ptr a) -> ForeignPtr a
unsafeFromAcquirePtr = unsafePerformIO . fromAcquirePtr

-- | Version of `unsafeFromAcquire` which returns a `WaterfallError` if the action throws
{-# NOINLINE unsafeFromAcquirePtrWithCatch #-}
unsafeFromAcquirePtrWithCatch :: Acquire (Ptr a) -> Either WaterfallError (ForeignPtr a)
unsafeFromAcquirePtrWithCatch = 
    left WaterfallError . unsafePerformIO . try . fromAcquirePtr

-- | Version of `unsafeFromAcquirePtr`  which operates on a container of pointers
{-# NOINLINE unsafeFromAcquirePtrT #-}
unsafeFromAcquirePtrT :: (Traversable t) => Acquire (t (Ptr a))  -> t (ForeignPtr a)
unsafeFromAcquirePtrT = unsafePerformIO . fromAcquirePtrT

-- | Version of `unsafeFromAcquirePtrWithCatch`  which registers the finalizer on a container of pointers 
{-# NOINLINE unsafeFromAcquirePtrTWithCatch #-}
unsafeFromAcquirePtrTWithCatch :: (Traversable t) => Acquire (t (Ptr a))  -> Either WaterfallError (t (ForeignPtr a))
unsafeFromAcquirePtrTWithCatch = left WaterfallError . unsafePerformIO . try . fromAcquirePtrT

-- | Borrow the pointer held by a `ForeignPtr` for the duration of an `Acquire`
-- 
-- The `ForeignPtr` is kept alive (it's finalizer isn't run) until the resource is released.
acquireForeignPtr :: ForeignPtr a -> Acquire (Ptr a)
acquireForeignPtr fp =
    mkAcquire 
        (pure . unsafeForeignPtrToPtr $ fp)
        (const (touchForeignPtr fp))

-- | calling the pure valued functions with a pointer type would almost certainly be a bug
-- 
-- @Ptr@ has an @NFData@ instance, so if we _only_ used that constraint, the functions could be applied to pointers
--
-- Defining a type family for "things that aren't pointers" lets us check that we aren't acquiring pointers
type NoPtr :: k -> Constraint
type family NoPtr a where
    NoPtr (Ptr a) = TypeError ('Text "unsafeFromAcquire can't return a " ':<>: 'ShowType (Ptr a) ':$$: 'Text " use unsafeFromAcquirePtr instead")
    NoPtr (ForeignPtr a) = TypeError ('Text "unsafeFromAcquire can't return a " ':<>: 'ShowType (ForeignPtr a) ':$$: 'Text " use unsafeFromAcquirePtr instead")
    NoPtr (f a) = (NoPtr f, NoPtr a)
    NoPtr _ = ()

-- | Run a resource in the `Data.Acquire.Acquire` monad to completion,
-- fully evaluating its result before the resource is released.
--
-- Because the result is forced with `force`, it shouldn't hold on to anything inside the resource,
-- making it safe to release the resource immediately.
runAcquire :: (NFData a, NoPtr a) => Acquire a -> IO a
runAcquire a = with a (evaluate . force)


-- | Run a resource in the `Data.Acquire.Acquire` monad, returning its (fully evaluated) result as a pure value.
--
-- Analagous to calling `unsafePerformIO` to extract a value in the `IO` monad.
-- The same constraints as apply to `unsafePerformIO` apply to this method. 
-- That is, it should only be used on "philosophically pure" actions.
--
-- The resource is released as soon as the result has been computed.
-- If the result needs to hold a pointer into the resource, use `unsafeFromAcquirePtr` instead.
{-# NOINLINE unsafeFromAcquire #-}
unsafeFromAcquire :: (NFData a, NoPtr a) => Acquire a -> a
unsafeFromAcquire = unsafePerformIO . runAcquire

-- | Version of `unsafeFromAcquire` which returns a `WaterfallError` if the action throws
{-# NOINLINE unsafeFromAcquireWithCatch #-}
unsafeFromAcquireWithCatch :: (NFData a, NoPtr a) => Acquire a -> Either WaterfallError a
unsafeFromAcquireWithCatch = 
    left WaterfallError . unsafePerformIO . try . runAcquire
