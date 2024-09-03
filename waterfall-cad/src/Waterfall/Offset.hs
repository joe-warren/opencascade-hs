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
-- This is based on @MakeOffsetShape@ from the underlying OpenCascade library.
-- And as such, only supports the same set of `Solid`s that @MakeOffsetShape@ does.
--
-- The documentation for @MakeOffsetShape@ lists the following constraints
-- ( [link](https://dev.opencascade.org/doc/refman/html/class_b_rep_offset_a_p_i___make_offset_shape.html) ):
--
-- * All the faces of the shape S should be based on the surfaces with continuity at least C1.
-- * The offset value should be sufficiently small to avoid self-intersections in resulting shape.
--      Otherwise these self-intersections may appear inside an offset face if its initial surface is not plane or sphere or cylinder, also some non-adjacent offset faces may intersect each other. Also, some offset surfaces may "turn inside out".
-- * The algorithm may fail if the shape S contains vertices where more than 3 edges converge.
-- * Since 3d-offset algorithm involves intersection of surfaces, it is under limitations of surface intersection algorithm.
-- * A result cannot be generated if the underlying geometry of S is BSpline with continuity C0.
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