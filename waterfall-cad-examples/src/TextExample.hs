{-|
<<models/text.glb>>
-}
module TextExample (
    textExample,
    playgroundTextExample
) where

import qualified Waterfall
import Linear (unit, _x)

textExample :: FilePath -> Double -> String -> Double -> IO Waterfall.Solid
textExample fontpath fontSize content depth = do
    font <- Waterfall.fontFromPath fontpath fontSize
    return . Waterfall.rotate (unit _x) (pi/2) . Waterfall.prism depth $ Waterfall.text font content

-- | This is intended to be viewed via the WASM playground
playgroundTextExample :: IO Waterfall.Solid
playgroundTextExample = textExample "/VarelaRound-Regular.ttf" 24 "Waterfall-CAD" 12 