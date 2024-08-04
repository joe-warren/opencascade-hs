module Waterfall.TwoD.Internal.Path2D
( Path2D (..)
, joinPaths
) where

import Data.Foldable (traverse_, toList)
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import Foreign.Ptr
import Data.Semigroup (sconcat)
import Waterfall.Internal.Edges (intersperseLines, joinWires)

-- | A Path in 2D Space 
--
-- Under the hood, this is represented by an OpenCascade `TopoDS.Wire`, constrained to the plane \(z=0\).
--
-- Please feel free to report a bug if you're able to construct a `Path2D`
-- which does not lie on this plane (without using Internal functions).
newtype Path2D = Path2D { rawPath :: Ptr TopoDS.Wire }

joinPaths :: [Path2D] -> Path2D
joinPaths paths = Path2D . unsafeFromAcquire $ do
    wires <- traverse (toAcquire . rawPath) paths
    joinWires =<< intersperseLines wires

-- | Joins `Path2D`s, @ a <> b @ connects the end point of @ b @ to the start of @ b @, if these points are not coincident, a line is created between them.
-- 
-- Attempts to combine paths in ways that generate a non manifold path will produce an error case that is not currently handled gracefully.
instance Semigroup Path2D where
    sconcat = joinPaths . toList
    a <> b = joinPaths [a, b] 
    
instance Monoid Path2D where
    mempty = joinPaths []
    mconcat = joinPaths