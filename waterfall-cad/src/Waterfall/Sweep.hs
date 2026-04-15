module Waterfall.Sweep
( sweep
) where

import Waterfall.Internal.Solid (Solid (..), acquireSolid, solidFromAcquire)
import Waterfall.Internal.Path (Path (..))
import Waterfall.Internal.Path.Common (RawPath (..))
import Waterfall.Internal.Edges (allWires)
import Waterfall.Internal.Finalizers (toAcquire)
import Waterfall.TwoD.Internal.Shape (Shape (..))
import qualified OpenCascade.BRepOffsetAPI.MakePipeShell as MakePipeShell
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import OpenCascade.Inheritance (upcast)
import Control.Monad.IO.Class (liftIO)
import qualified Waterfall.Solids as Solids

-- | Sweep a 2D `Shape` along a `Path`, constructing a `Solid`
sweep :: Path -> Shape -> Solid
sweep (Path (ComplexRawPath theRawPath)) (Shape theRawShape) = solidFromAcquire $ do
    path <- toAcquire theRawPath
    shape <- toAcquire theRawShape
    wires <- allWires shape
    case wires of
        (profileWire : _) -> do
            builder <- MakePipeShell.new path
            liftIO $ do
                MakePipeShell.setModeFrenet builder True
                MakePipeShell.add builder profileWire True True
                MakeShape.build (upcast builder)
                _ <- MakePipeShell.makeSolid builder
                pure ()
            MakeShape.shape (upcast builder)
        [] -> acquireSolid Solids.emptySolid
sweep _ _ = Solids.emptySolid
