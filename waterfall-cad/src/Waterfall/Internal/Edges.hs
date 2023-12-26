module Waterfall.Internal.Edges
( gpPntToV3
, edgeEndpoints
, wireEndpoints
, wireTangent
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
import qualified OpenCascade.GP.Vec as GP.Vec


gpPntToV3 :: Ptr GP.Pnt -> IO (V3 Double)
gpPntToV3 pnt = V3 <$> GP.Pnt.getX pnt <*> GP.Pnt.getY pnt <*> GP.Pnt.getZ pnt

gpVecToV3 :: Ptr GP.Vec -> IO (V3 Double)
gpVecToV3 vec = V3 <$> GP.Vec.getX vec <*> GP.Vec.getY vec <*> GP.Vec.getZ vec

edgeEndpoints :: Ptr TopoDS.Edge -> IO (V3 Double, V3 Double)
edgeEndpoints edge = (`with` pure) $ do
    curve <- BRep.Tool.curve edge
    p1 <- liftIO . BRep.Tool.curveParamFirst $ edge
    p2 <- liftIO . BRep.Tool.curveParamLast $ edge
    s <- (liftIO . gpPntToV3) =<< Geom.Curve.value curve p1
    e <- (liftIO . gpPntToV3) =<< Geom.Curve.value curve p2
    return (s, e)

wireEndpoints :: Ptr TopoDS.Wire -> IO (V3 Double, V3 Double)
wireEndpoints wire = with (WireExplorer.fromWire wire) $ \explorer -> do
    v1 <- WireExplorer.current explorer
    (s, _) <- edgeEndpoints v1
    let runToEnd = do
            edge <- WireExplorer.current explorer
            (_s, e') <- edgeEndpoints edge
            WireExplorer.next explorer
            more <- WireExplorer.more explorer
            if more 
                then runToEnd
                else pure e'
    e <- runToEnd
    return (s, e)


edgeTangent :: Ptr TopoDS.Edge -> IO (V3 Double)
edgeTangent e = (`with` pure) $ do
    curve <- BRep.Tool.curve e
    p1 <- liftIO . BRep.Tool.curveParamFirst $ e
    liftIO . gpVecToV3 =<< Geom.Curve.dn curve p1 1

wireTangent :: Ptr TopoDS.Wire -> IO (V3 Double)
wireTangent wire = with (WireExplorer.fromWire wire) $ \explorer -> do
    v1 <- WireExplorer.current explorer
    edgeTangent v1