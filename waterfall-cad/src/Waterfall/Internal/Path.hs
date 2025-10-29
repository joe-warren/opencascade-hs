{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Waterfall.Internal.Path
( Path (..)
, allPathEndpoints
) where

import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire) 
import Control.Monad.IO.Class (liftIO)
import Linear (V3 (..))
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.Internal.Edges (allWireEndpoints)
-- | A Path in 3D Space 
--
-- Under the hood, this is represented by an OpenCascade `TopoDS.Wire`.
--
-- The monoid instance  Joins `Path`s, @ a <> b @ connects the end point of @ a @ to the start of @ b @, if these points are not coincident, a line is created between them.
--
newtype Path = Path { rawPath :: RawPath } deriving (Semigroup, Monoid) via RawPath

-- | Exposing this because I found it useful for debugging
allPathEndpoints :: Path -> [(V3 Double, V3 Double)]
allPathEndpoints (Path (ComplexRawPath raw)) = unsafeFromAcquire $ do
    wire <- toAcquire raw
    liftIO $ allWireEndpoints wire
allPathEndpoints (Path (SinglePointRawPath point)) = pure (point, point)
allPathEndpoints (Path EmptyRawPath) = []