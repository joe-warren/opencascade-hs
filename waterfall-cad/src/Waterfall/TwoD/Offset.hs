module Waterfall.TwoD.Offset
( offsetShape
, tryOffsetShape
) where
import Waterfall.TwoD.Internal.Shape (Shape (..))
import Waterfall.Error (WaterfallError)
import Waterfall.Internal.Finalizers (acquireForeignPtr, unsafeFromAcquirePtrTWithCatch)
import Data.Either (fromRight)
import Waterfall.Internal.NearZero (nearZero)
import Waterfall.Internal.FromOpenCascade (subShapes)
import Foreign (Ptr)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import Data.Acquire (Acquire)
import OpenCascade.Inheritance (upcast)
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.BRepOffsetAPI.MakeOffset as MakeOffset
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.BRepTools as BRepTools
import Control.Monad (forM, filterM)
import qualified Waterfall.Booleans as Booleans
import Data.Functor.Compose (Compose(..))
import qualified OpenCascade.BRepBuilderAPI.MakeFace as MakeFace
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified OpenCascade.GP.Pln as GP.Pln

offsetShape :: Double -> Shape -> Shape
offsetShape amount s = fromRight mempty $ tryOffsetShape amount s

offsetOne :: Double -> Ptr TopoDS.Face -> Acquire (NonEmpty [Ptr TopoDS.Shape])
offsetOne amount f = do
    outer <- BRepTools.outerWire f
    wires <- subShapes (upcast f)
    inner <- filterM (liftIO . fmap not . TopoDS.Shape.isSame (upcast outer) . upcast) wires
    pln <- GP.Pln.new
    let wiresAndAmounts = (outer :| inner) `NE.zip` (amount :| repeat (negate amount))
    forM wiresAndAmounts $ \(wire, amount') -> do
        builder <- MakeOffset.new
        liftIO $ MakeOffset.addWire builder wire
        liftIO $ MakeOffset.perform builder amount' 0.0
        done <- liftIO . MakeShape.isDone . upcast $ builder

        if not done
            then return []
            else do 
                outShape <- MakeShape.shape (upcast builder)
                outWires <- subShapes outShape
                forM outWires $ \wire -> 
                    (MakeShape.shape . upcast)
                    =<< MakeFace.fromPlnAndWire pln wire True

tryOffsetShape :: Double -> Shape -> Either WaterfallError Shape
tryOffsetShape amount (Shape s)
    | nearZero amount = Right (Shape s)
    | otherwise =
        fmap 
            (mconcat 
            . fmap 
                (foldl1 Booleans.difference
                . fmap mconcat 
                . getCompose
                )
            . getCompose 
            . fmap Shape
            )
        . unsafeFromAcquirePtrTWithCatch
        . fmap (Compose . fmap Compose)
        $ traverse (offsetOne amount)
        =<< subShapes
        =<< acquireForeignPtr s
