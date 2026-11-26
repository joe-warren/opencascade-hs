module SVG.PathExample 
( pathExample 
) where

import qualified Waterfall.SVG
import qualified Waterfall.Solid as Solid
import qualified Waterfall.TwoD.Shape as Shape
import Data.Bifunctor (Bifunctor(bimap))

pathExample :: String -> Either String Solid.Solid
pathExample pathStr =
    let solidify = mconcat . fmap (Solid.prism 1 . Shape.makeShape)
    in bimap show solidify $ Waterfall.SVG.parsePath pathStr