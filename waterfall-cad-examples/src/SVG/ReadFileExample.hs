module SVG.ReadFileExample 
( readFileExample 
) where

import qualified Waterfall.SVG
import qualified Waterfall.Solids as Solids
import qualified Waterfall.TwoD.Shape as Shape

readFileExample :: FilePath -> IO Solids.Solid
readFileExample filepath =
    let solidify = mconcat . fmap (Solids.prism 1 . Shape.fromPath)
    in either (error . show) solidify <$> Waterfall.SVG.readSVG filepath