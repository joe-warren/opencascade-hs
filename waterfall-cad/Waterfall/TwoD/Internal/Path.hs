module Waterfall.TwoD.Internal.Path
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
-- TopoDS_Wire and TopoDS_Face represent shapes in _3d space_
-- however, we're constraining this to the plane `z = 0`
newtype Path = Path { runPath :: Acquire (Ptr TopoDS.Wire) }

joinPaths :: [Path] -> Path
joinPaths paths = Path $ do
    builder <- MakeWire.new
    traverse_ (liftIO . MakeWire.addWire builder <=< runPath) paths
    MakeWire.wire builder
    
