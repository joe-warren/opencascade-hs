module SVG.PathExample 
( pathExample 
) where

import qualified Waterfall.SVG
import qualified Waterfall.Solids as Solids
import qualified Waterfall.TwoD.Shape as Shape
import Data.Bifunctor (Bifunctor(bimap))

pathExample :: String -> Either String Solids.Solid
pathExample pathStr =
    let solidify = mconcat . fmap (Solids.prism 1 . Shape.makeShape)
    in bimap show solidify $ Waterfall.SVG.parsePath pathStr