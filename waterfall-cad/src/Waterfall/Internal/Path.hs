{-# LANGUAGE  InstanceSigs#-}
{-# OPTIONS_HADDOCK not-home #-}
module Waterfall.Internal.Path
( Path (..)
, joinPaths
, allPathEndpoints
) where

import Data.List.NonEmpty (NonEmpty ())
import Data.Foldable (toList)
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire) 
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.Ptr
import Linear (V3 (..))
import Data.Semigroup (sconcat)
import Waterfall.Internal.Edges (allWireEndpoints, intersperseLines, joinWires)
-- | A Path in 3D Space 
--
-- Under the hood, this is represented by an OpenCascade `TopoDS.Wire`.
newtype Path = Path { rawPath :: Ptr TopoDS.Wire }

-- | Exposing this because I found it useful for debugging
allPathEndpoints :: Path -> [(V3 Double, V3 Double)]
allPathEndpoints (Path raw) = unsafeFromAcquire $ do
    wire <- toAcquire raw
    liftIO $ allWireEndpoints wire
    
joinPaths :: [Path] -> Path
joinPaths paths = Path . unsafeFromAcquire $ do
    wires <- traverse (toAcquire . rawPath) paths
    joinWires =<< intersperseLines wires

-- | Joins `Path`s, @ a <> b @ connects the end point of @ b @ to the start of @ b @, if these points are not coincident, a line is created between them.
-- 
-- Attempts to combine paths in ways that generate a non manifold path will produce an error case that is not currently handled gracefully.
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