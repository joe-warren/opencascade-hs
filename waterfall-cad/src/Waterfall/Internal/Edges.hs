module Waterfall.Internal.Edges
( gpPntToV3
, edgeEndpoints
, wireEndpoints
) where

import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.BRep.Tool as BRep.Tool
import qualified OpenCascade.Geom.Curve as Geom.Curve
import qualified OpenCascade.BRepTools.WireExplorer as WireExplorer
import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.GP as GP
import Data.Acquire
import Control.Monad.IO.Class (liftIO)
import Linear (V3 (..))
import Foreign.Ptr


gpPntToV3 :: Ptr GP.Pnt -> IO (V3 Double)
gpPntToV3 pnt = V3 <$> GP.Pnt.getX pnt <*> GP.Pnt.getY pnt <*> GP.Pnt.getZ pnt

edgeEndpoints :: Ptr TopoDS.Edge -> IO (V3 Double, V3 Double)
edgeEndpoints e = (`with` pure) $ do
    curve <- BRep.Tool.curve e
    p1 <- liftIO . BRep.Tool.curveParamFirst $ e
    p2 <- liftIO . BRep.Tool.curveParamLast $ e
    s <- (liftIO . gpPntToV3) =<< Geom.Curve.value curve p1
    e <- (liftIO . gpPntToV3) =<< Geom.Curve.value curve p2
    return (s, e)

wireEndpoints :: Ptr TopoDS.Wire -> IO (V3 Double, V3 Double)
wireEndpoints wire = with (WireExplorer.fromWire wire) $ \explorer -> do
    v1 <- WireExplorer.current explorer
    (s, _) <- edgeEndpoints v1
    let runToEnd = do
            edge <- WireExplorer.current explorer
            (s, e') <- edgeEndpoints edge
            WireExplorer.next explorer
            more <- WireExplorer.more explorer
            if more 
                then runToEnd
                else pure e'
    e <- runToEnd
    return (s, e)