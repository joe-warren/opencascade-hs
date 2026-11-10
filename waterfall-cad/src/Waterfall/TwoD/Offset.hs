module Waterfall.TwoD.Offset
( offsetShape
, tryOffsetShape
) where
import Waterfall.TwoD.Internal.Shape (Shape (..))
import Waterfall.Error (WaterfallError)
import Waterfall.Internal.Finalizers (unsafeFromAcquirePtrWithCatch, acquireForeignPtr, unsafeFromAcquirePtrTWithCatch)
import Data.Either (fromRight)
import Waterfall.Internal.NearZero (nearZero)
import Waterfall.Internal.Edges (allSubShapesWithCopy)
import Foreign (Ptr)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP.Pln as GP.Pln
import Data.Acquire (Acquire)
import OpenCascade.Inheritance (unsafeDowncast, upcast)
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.BRepBuilderAPI.MakeFace as MakeFace
import qualified OpenCascade.BRepOffsetAPI.MakeOffset as MakeOffset
import qualified OpenCascade.GeomAbs.JoinType as GeomAbs.JointType
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import Control.Monad (forM_, join, when)
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.BOPAlgo.Tools as BOPAlgo.Tools



offsetShape :: Double -> Shape -> Shape
offsetShape amount s = fromRight mempty $ tryOffsetShape amount s

getCompoundAsFaces :: Ptr TopoDS.Shape -> Acquire [Ptr TopoDS.Face]
getCompoundAsFaces s = 
    traverse (liftIO . unsafeDowncast)
        =<< allSubShapesWithCopy ShapeEnum.Face s 

offsetOne :: Double -> Ptr TopoDS.Face -> Acquire [Ptr TopoDS.Shape]
offsetOne amount f = do
    builder <- MakeOffset.new
    liftIO $ MakeOffset.init builder f GeomAbs.JointType.Arc False
    wires <- traverse (liftIO . unsafeDowncast) =<< allSubShapesWithCopy ShapeEnum.Wire (upcast f)
    forM_ wires (liftIO . MakeOffset.addWire builder)
    liftIO $ MakeOffset.perform builder amount 0.0
    outWires <- MakeShape.shape (upcast builder)
    let angularTolerance = 1e-8
    outFaces <- TopoDS.Shape.new
    ok <- liftIO $ BOPAlgo.Tools.wiresToFaces outWires outFaces angularTolerance
    if not ok 
        then return []
        else 
            allSubShapesWithCopy ShapeEnum.Face outFaces

tryOffsetShape :: Double -> Shape -> Either WaterfallError Shape
tryOffsetShape amount (Shape s)
    | nearZero amount = Right (Shape s)
    | otherwise =
        fmap (mconcat . fmap Shape)
        . unsafeFromAcquirePtrTWithCatch
        . fmap join
        $ traverse (offsetOne amount)
        =<< getCompoundAsFaces
        =<< acquireForeignPtr s
