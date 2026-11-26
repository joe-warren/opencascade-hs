{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Waterfall.Internal.FromOpenCascade
( gpPntToV3
, gpVecToV3
, gpXYZToV3
, shapesFromListOfShape
, deduplicateTopoDSShapes
, containsShapeKind
, HasShapeKind (shapeKind)
, subShapes
) where

import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.GP.Vec as GP.Vec
import qualified OpenCascade.GP.XYZ as GP.XYZ
import qualified OpenCascade.GP as GP
import qualified OpenCascade.NCollection as NCollection
import qualified OpenCascade.NCollection.List.Iterator as NCollection.List.Iterator
import qualified OpenCascade.TopoDS as TopoDS 
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import Linear (V3 (..))
import Foreign.Ptr
import Data.Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)
import Waterfall.Internal.Finalizers (unsafeFromAcquire, NoPtr)
import qualified Waterfall.Internal.ShapeMap as ShapeMap
import Data.Maybe (isJust)
import Data.Foldable (foldrM)
import Control.DeepSeq (NFData)
import OpenCascade.TopAbs (ShapeEnum)
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.TopExp.Explorer as Explorer
import Control.Monad ((<=<))
import OpenCascade.Inheritance (unsafeDowncast, DiscriminatedSubTypeOf)
import Data.Proxy (Proxy (Proxy))
    
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

deduplicateTopoDSShapes :: (NFData a, NoPtr a) => (a -> Acquire (Ptr TopoDS.Shape)) -> [a] -> [a]
deduplicateTopoDSShapes toShape ss = 
    let f s (shapeMap, acc)  = do
            shape <- toShape s
            inMap <-  liftIO $ isJust <$> ShapeMap.lookup shapeMap shape
            if inMap
                then pure (shapeMap, acc)
                else
                    liftIO $ (, s:acc) <$> ShapeMap.addIfAbsent (shape, ()) shapeMap
    in unsafeFromAcquire (snd <$> foldrM f (ShapeMap.empty, []) ss)


containsShapeKind :: ShapeEnum -> Ptr TopoDS.Shape -> Acquire Bool
containsShapeKind kind = 
    liftIO . Explorer.more <=< (`Explorer.new` kind)

class HasShapeKind a where
    shapeKind :: Proxy a -> ShapeEnum

instance HasShapeKind TopoDS.Face where
    shapeKind _ = ShapeEnum.Face

instance HasShapeKind TopoDS.Wire where
    shapeKind _ = ShapeEnum.Wire

instance HasShapeKind TopoDS.Edge where
    shapeKind _ = ShapeEnum.Edge

instance HasShapeKind TopoDS.Shell where
    shapeKind _ = ShapeEnum.Shell

instance HasShapeKind TopoDS.Solid where
    shapeKind _ = ShapeEnum.Solid

instance HasShapeKind TopoDS.Vertex where
    shapeKind _ = ShapeEnum.Vertex

subShapes :: forall a. (HasShapeKind a, DiscriminatedSubTypeOf TopoDS.Shape a) => Ptr TopoDS.Shape -> Acquire [Ptr a]
subShapes s = do 
    explorer <- Explorer.new s (shapeKind (Proxy @a))
    let go acc = do
            isMore <- liftIO $ Explorer.more explorer
            if isMore 
                then do
                    v <- liftIO $ Explorer.value explorer
                    isPresent <- liftIO $ ShapeMap.lookup acc v
                    (newAcc, add) <- case isPresent of 
                        Nothing -> do 
                            v' <- TopoDS.Shape.copy v
                            v'' <- liftIO $ unsafeDowncast v'
                            newMap <- liftIO $ ShapeMap.addIfAbsent (v', ()) acc
                            return (newMap, (v'':))
                        Just _ -> pure (acc, id) 
                    liftIO $ Explorer.next explorer
                    add <$> go newAcc
                else return []
    go ShapeMap.empty