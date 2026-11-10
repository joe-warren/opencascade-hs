module Waterfall.TwoD.Offset
( offsetShape
, tryOffsetShape
) where
import Waterfall.TwoD.Internal.Shape (Shape (..))
import Waterfall.Error (WaterfallError)
import Waterfall.Internal.Finalizers (acquireForeignPtr, unsafeFromAcquirePtrTWithCatch)
import Data.Either (fromRight)
import Waterfall.Internal.NearZero (nearZero)
import Waterfall.Internal.Edges (allSubShapesWithCopy)
import Foreign (Ptr)
import qualified OpenCascade.TopoDS as TopoDS
import Data.Acquire (Acquire)
import OpenCascade.Inheritance (unsafeDowncast, upcast)
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.BRepOffsetAPI.MakeOffset as MakeOffset
import qualified OpenCascade.GeomAbs.JoinType as GeomAbs.JointType
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import Control.Monad (forM_, forM, (<=<))
import qualified Waterfall.Booleans as Booleans
import Data.Functor.Compose (Compose(..))
import qualified OpenCascade.BRepBuilderAPI.MakeFace as MakeFace

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
    done <- liftIO . MakeShape.isDone . upcast $ builder

    if not done
        then return []
        else do 
            outShape <- MakeShape.shape (upcast builder)
            outWires <- traverse (liftIO . unsafeDowncast) =<< allSubShapesWithCopy ShapeEnum.Wire outShape
            forM outWires 
                ((MakeShape.shape . upcast) <=< (`MakeFace.fromWire` False))

xOr :: Shape -> Shape -> Shape
a `xOr` b = (a `Booleans.difference` b) <> (b `Booleans.difference` a)

tryOffsetShape :: Double -> Shape -> Either WaterfallError Shape
tryOffsetShape amount (Shape s)
    | nearZero amount = Right (Shape s)
    | otherwise =
        fmap (mconcat . fmap (foldl' xOr mempty) . getCompose . fmap Shape)
        . unsafeFromAcquirePtrTWithCatch
        . fmap Compose
        $ traverse (offsetOne amount)
        =<< getCompoundAsFaces
        =<< acquireForeignPtr s
