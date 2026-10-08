{-# LANGUAGE DeriveTraversable #-}
module Waterfall.Internal.ShapeMap
( ShapeMap
, lookup
, fromList
, addIfAbsent
, empty
) where
import Prelude hiding (lookup)
import qualified OpenCascade.TopoDS as TopoDS
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Foreign.Ptr (Ptr)
import qualified OpenCascade.TopTools.ShapeMapHasher as TopTools.ShapeMapHasher
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import Control.Monad (forM, join)

newtype ShapeMap a = ShapeMap 
    { getInternalMap :: IntMap [(Ptr TopoDS.Shape, a)] 
    } deriving (Functor, Foldable, Traversable)

empty :: ShapeMap a
empty = ShapeMap mempty

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)

lookupList :: Ptr TopoDS.Shape -> [(Ptr TopoDS.Shape, a)] -> IO (Maybe a)
lookupList s = fmap (fmap snd) . findM (TopoDS.Shape.isSame s . fst) 

lookup :: ShapeMap a -> Ptr TopoDS.Shape -> IO (Maybe a)
lookup (ShapeMap hashMap) s = do
    hash <- TopTools.ShapeMapHasher.hash s
    fmap join . forM (IntMap.lookup hash hashMap) $ lookupList s

fromList :: [(Ptr TopoDS.Shape, a)] -> IO (ShapeMap a)
fromList = foldr (\value -> (addIfAbsent value =<<)) (pure empty)

addIfAbsent :: (Ptr TopoDS.Shape, a) -> ShapeMap a -> IO (ShapeMap a)
addIfAbsent (s, v) (ShapeMap hashMap) = do 
    hash <- TopTools.ShapeMapHasher.hash s
    let update Nothing = pure (Just [(s, v)])
        update (Just vs) = do
            r <- lookupList s vs
            return $ case r of 
                Nothing -> Just ((s, v): vs)
                Just _ -> Just vs
    ShapeMap <$> IntMap.alterF update hash hashMap 