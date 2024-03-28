module Waterfall.Offset 
( offset
) where 

import Waterfall.Internal.Solid (Solid (..), acquireSolid, solidFromAcquire)
import qualified OpenCascade.BRepOffsetAPI.MakeOffsetShape as MakeOffsetShape
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Inheritance (SubTypeOf(upcast), unsafeDowncast)
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.BRepOffset.Mode as Mode
import qualified OpenCascade.GeomAbs.JoinType as GeomAbs.JoinType
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as MakeSolid
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.TopExp.Explorer as TopExp.Explorer
import qualified OpenCascade.TopAbs.ShapeEnum as TopAbs.ShapeEnum
import Control.Monad (when)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire)
import Linear.Epsilon (nearZero)

combineShellsToSolid :: Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)
combineShellsToSolid s = do
    explorer <- TopExp.Explorer.new s TopAbs.ShapeEnum.Shell
    makeSolid <- MakeSolid.new
    let go = do
            isMore <- liftIO $ TopExp.Explorer.more explorer
            when isMore $ do
                shell <- liftIO $ unsafeDowncast =<< TopExp.Explorer.value explorer
                liftIO $ MakeSolid.add makeSolid shell
                liftIO $ TopExp.Explorer.next explorer
                go
    go
    upcast <$> MakeSolid.solid makeSolid

-- | Expand or contract a `Solid` by a certain amount.
-- 
-- This seems to be relatively fragile on Sweeps/Prisms
-- (as in, may fail to produce a result)
offset :: Double    -- ^ Amount to offset by, positive values expand, negative values contract
    -> Double       -- ^ Tolerance, this can be relatively small
    -> Solid        -- ^ the `Solid` to offset 
    -> Solid
offset value tolerance solid
    | nearZero value = solid
    | otherwise = 
  solidFromAcquire $ do
    builder <- MakeOffsetShape.new
    s <- acquireSolid solid 
    --liftIO $ MakeOffsetShape.performBySimple builder s value
    liftIO $ MakeOffsetShape.performByJoin builder s value tolerance Mode.Skin False False GeomAbs.JoinType.Arc False 
    shell <- MakeShape.shape (upcast builder)
    combineShellsToSolid shell