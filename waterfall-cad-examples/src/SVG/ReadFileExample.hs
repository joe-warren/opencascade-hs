module SVG.ReadFileExample 
( readFileExample 
) where

import qualified Waterfall.SVG
import qualified Waterfall.Solid as Solid
import qualified Waterfall.TwoD.Shape as Shape
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Booleans as Booleans
import Linear (V3 (..))

readFileExample :: FilePath -> IO Solid.Solid
readFileExample filepath =
    let expandVertically = Transforms.translate (V3 0 0 (-0.5)) . Transforms.scale (V3 1 1 2)
        xor a b = (a `Booleans.difference` expandVertically b) <> (b `Booleans.difference` expandVertically a)
        solidify = foldr xor Solid.emptySolid . fmap (Solid.prism 1 . Shape.makeShape)
    in either (error . show) solidify <$> Waterfall.SVG.readSVG filepath