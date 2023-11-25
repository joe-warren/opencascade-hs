module Waterfall.TwoD.Internal.Path2D
( Path2D (..)
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

-- TopoDS_Wire and TopoDS_Face represent shapes in _3d space_
-- however, we're constraining this to the plane `z = 0`
newtype Path2D = Path2D { runPath :: Acquire (Ptr TopoDS.Wire) }

joinPaths :: [Path2D] -> Path2D
joinPaths paths = Path2D $ do
    builder <- MakeWire.new
    traverse_ (liftIO . MakeWire.addWire builder <=< runPath) paths
    MakeWire.wire builder

instance Semigroup Path2D where
    sconcat = joinPaths . toList
    
instance Monoid Path2D where
    mempty = joinPaths []
    mconcat = joinPaths