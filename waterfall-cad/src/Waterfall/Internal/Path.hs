module Waterfall.Internal.Path
( Path (..)
, joinPaths
) where

import Data.Foldable (traverse_)
import Data.Acquire
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import Foreign.Ptr
import Data.Foldable (toList)
import Data.Semigroup (sconcat)

newtype Path = Path { runPath :: Acquire (Ptr TopoDS.Wire) }

joinPaths :: [Path] -> Path
joinPaths paths = Path $ do
    builder <- MakeWire.new
    traverse_ (liftIO . MakeWire.addWire builder <=< runPath) paths
    MakeWire.wire builder

instance Semigroup Path where
    sconcat = joinPaths . toList
    
instance Monoid Path where
    mempty = joinPaths []
    mconcat = joinPaths