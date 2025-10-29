{-# LANGUAGE  InstanceSigs #-}
module Waterfall.Internal.Path.Common 
( RawPath (..)
, joinRawPaths
, rawPathWire
) where

import Data.Acquire
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import qualified OpenCascade.BRepBuilderAPI.MakeEdge as MakeEdge
import Waterfall.Internal.Edges (joinWires, wireEndpoints)
import Waterfall.Internal.ToOpenCascade (v3ToPnt)
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire)
import Control.Monad.IO.Class (liftIO)
import Linear (V3 (..), distance)
import Foreign.Ptr
import Data.Maybe (catMaybes)
import Data.Semigroup (sconcat)
import Data.List.NonEmpty (NonEmpty ())
import Data.Foldable (toList)

data RawPath = EmptyRawPath | SinglePointRawPath (V3 Double) | ComplexRawPath (Ptr TopoDS.Wire)

rawPathWire :: RawPath -> Maybe (Ptr TopoDS.Wire)
rawPathWire (ComplexRawPath wire) = Just wire
rawPathWire _ = Nothing

rawPathToEither :: RawPath -> Maybe (Either (V3 Double) (Ptr TopoDS.Wire))
rawPathToEither EmptyRawPath = Nothing
rawPathToEither (SinglePointRawPath p) = Just . Left $ p
rawPathToEither (ComplexRawPath wire) = Just . Right $ wire

line' :: V3 Double -> V3 Double -> Acquire (Ptr TopoDS.Wire)
line' s e = do
    builder <- MakeWire.new
    pt1 <- v3ToPnt s
    pt2 <- v3ToPnt e
    edge <- MakeEdge.fromPnts pt1 pt2
    liftIO $ MakeWire.addEdge builder edge
    MakeWire.wire builder
    
intersperseLines :: [Either (V3 Double) (Ptr TopoDS.Wire)] -> Acquire [Ptr TopoDS.Wire]
intersperseLines [] = pure []
intersperseLines [Left _x] = pure []
intersperseLines [Right x] = pure [x]
intersperseLines (a:b:xs) = do
    ea <- case a of 
            Left pnt -> pure pnt
            Right wire -> do
                wire' <- toAcquire wire
                liftIO $ snd <$> wireEndpoints wire'
    sb <- case b of
            Left pnt -> pure pnt 
            Right wire -> liftIO $ fst <$> wireEndpoints wire
    let prependA = either (const id) (:) a
    if distance ea sb < 1e-6
            then prependA <$> intersperseLines (b:xs)
            else prependA <$> ((:) <$> line' ea sb <*> intersperseLines (b:xs))

joinRawPaths :: [RawPath] -> RawPath
joinRawPaths paths = 
    case catMaybes (rawPathToEither <$> paths) of
        [] -> EmptyRawPath
        [Left pnt] -> SinglePointRawPath pnt
        path@(h:_) -> unsafeFromAcquire $ do
            interspersed <- intersperseLines path
            case interspersed of 
                [] -> pure . either SinglePointRawPath ComplexRawPath $ h
                wires -> ComplexRawPath <$> joinWires wires

instance Semigroup RawPath where
    sconcat :: NonEmpty RawPath -> RawPath
    sconcat = joinRawPaths . toList
    (<>) :: RawPath -> RawPath -> RawPath
    a <> b = joinRawPaths [a, b]
    
instance Monoid RawPath where
    mempty :: RawPath
    mempty = EmptyRawPath
    mconcat :: [RawPath] -> RawPath
    mconcat = joinRawPaths

