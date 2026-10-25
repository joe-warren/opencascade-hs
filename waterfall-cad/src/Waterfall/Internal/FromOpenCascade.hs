module Waterfall.Internal.FromOpenCascade
( gpPntToV3
, gpVecToV3
, gpXYZToV3
, shapesFromListOfShape
) where

import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.GP.Vec as GP.Vec
import qualified OpenCascade.GP.XYZ as GP.XYZ
import qualified OpenCascade.GP as GP
import qualified OpenCascade.NCollection as NCollection
import qualified OpenCascade.NCollection.List as NCollection.List
import qualified OpenCascade.NCollection.List.Iterator as NCollection.List.Iterator
import qualified OpenCascade.TopoDS as TopoDS 
import Linear (V3 (..))
import Foreign.Ptr
import Data.Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)
    
gpPntToV3 :: Ptr GP.Pnt -> IO (V3 Double)
gpPntToV3 pnt = V3 <$> GP.Pnt.getX pnt <*> GP.Pnt.getY pnt <*> GP.Pnt.getZ pnt

gpVecToV3 :: Ptr GP.Vec -> IO (V3 Double)
gpVecToV3 vec = V3 <$> GP.Vec.getX vec <*> GP.Vec.getY vec <*> GP.Vec.getZ vec

gpXYZToV3 :: Ptr GP.XYZ -> IO (V3 Double)
gpXYZToV3 xyz = V3 <$> GP.XYZ.x xyz <*> GP.XYZ.y xyz <*> GP.XYZ.z xyz


shapesFromListOfShape :: Ptr (NCollection.List TopoDS.Shape) -> Acquire [Ptr TopoDS.Shape]
shapesFromListOfShape list = do
    iterator <- NCollection.List.Iterator.newListOfShapeIterator list
    let go = do
            more <- liftIO $ NCollection.List.Iterator.moreListOfShapeIterator iterator
            if more
                then do
                    value <- NCollection.List.Iterator.valueListOfShapeIterator iterator
                    liftIO $ NCollection.List.Iterator.nextListOfShapeIterator iterator
                    (value :) <$> go
                else pure []
    go