{-# LANGUAGE  InstanceSigs#-}
{-# OPTIONS_HADDOCK not-home #-}
module Waterfall.Internal.Path
( Path (..)
, joinPaths
) where

import Data.List.NonEmpty (NonEmpty ())
import Data.Foldable (traverse_, toList)
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire) 
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import Foreign.Ptr
import Data.Semigroup (sconcat)

-- | A Path in 3D Space 
--
-- Under the hood, this is represented by an OpenCascade `TopoDS.Wire`.
newtype Path = Path { rawPath :: Ptr TopoDS.Wire }

joinPaths :: [Path] -> Path
joinPaths paths = Path . unsafeFromAcquire $ do
    builder <- MakeWire.new
    traverse_ (liftIO . MakeWire.addWire builder <=< toAcquire . rawPath) paths
    MakeWire.wire builder

-- | The Semigroup for `Path` attempts to join two paths that share a common endpoint.
--
-- Attempts to combine paths that do not share a common endpoint currently in an error case that is not currently handled gracefully.
instance Semigroup Path where
    sconcat :: NonEmpty Path -> Path
    sconcat = joinPaths . toList
    (<>) :: Path -> Path -> Path
    a <> b = joinPaths [a, b]
    
instance Monoid Path where
    mempty :: Path
    mempty = joinPaths []
    mconcat :: [Path] -> Path
    mconcat = joinPaths