module Waterfall.TwoD.Internal.Path2D
( Path2D (..)
, joinPaths
) where

import Data.Foldable (traverse_, toList)
import Data.Acquire
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import Foreign.Ptr
import Data.Semigroup (sconcat)

-- | A Path in 2D Space 
--
-- Under the hood, this is represented by an OpenCascade `TopoDS.Wire`, constrained to the plane \(z=0\).
--
-- Please feel free to report a bug if you're able to construct a `Path2D`
-- which does not lie on this plane (without using Internal functions).
newtype Path2D = Path2D { runPath :: Acquire (Ptr TopoDS.Wire) }

joinPaths :: [Path2D] -> Path2D
joinPaths paths = Path2D $ do
    builder <- MakeWire.new
    traverse_ (liftIO . MakeWire.addWire builder <=< runPath) paths
    MakeWire.wire builder

-- | The Semigroup for `Path2D` attempts to join two paths that share a common endpoint.
--
-- Attempts to combine paths that do not share a common endpoint currently in an error case that is not currently handled gracefully.
instance Semigroup Path2D where
    sconcat = joinPaths . toList
    a <> b = joinPaths [a, b] 
    
instance Monoid Path2D where
    mempty = joinPaths []
    mconcat = joinPaths