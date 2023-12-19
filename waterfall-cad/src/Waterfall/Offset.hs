module Waterfall.Offset 
( offset
) where 

import Waterfall.Internal.Solid (Solid (..))
import qualified OpenCascade.BRepOffsetAPI.MakeOffsetShape as MakeOffsetShape
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Inheritance (SubTypeOf(upcast), unsafeDowncast)
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.BRepOffset.Mode as Mode
import qualified OpenCascade.GeomAbs.JoinType as GeomAbs.JoinType
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as MakeSolid
import Linear.Epsilon (nearZero)

-- | Expand or contract a `Solid` by a certain amount.
-- 
-- This seems to be relatively fragile on Sweeps/Prisms
-- (as in, may fail to produce a result)
offset :: Double    -- ^ Amount to offset by, positive values expand, negative values contract
    -> Double       -- ^ Tolerance, this can be relatively small
    -> Solid        -- ^ the `Solid` to offset 
    -> Solid
offset value tolerance (Solid run)
    | nearZero value = Solid run
    | otherwise = 
  Solid $ do


    builder <- MakeOffsetShape.new
    s <- run 
    --liftIO $ MakeOffsetShape.performBySimple builder s value
    liftIO $ MakeOffsetShape.performByJoin builder s value tolerance Mode.Skin False False GeomAbs.JoinType.Arc False 
    shell <- MakeShape.shape (upcast builder)
    shellBuilder <- MakeSolid.new
    liftIO $ MakeSolid.add shellBuilder =<< unsafeDowncast shell
    MakeShape.shape (upcast shellBuilder)
    